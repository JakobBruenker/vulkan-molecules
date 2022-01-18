{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}

module Main where

import RIO
import RIO.ByteString qualified as B
import RIO.NonEmpty qualified as NE
import RIO.Text qualified as T
import RIO.Vector qualified as V
import Data.List.NonEmpty.Extra qualified as NE

import Control.Lens (views, (??), _1)
import Control.Monad.Extra (whileM)
import Data.Bits (Bits((.&.)))
import Options.Applicative
    ( (<**>),
      auto,
      fullDesc,
      help,
      long,
      metavar,
      option,
      short,
      showDefault,
      switch,
      execParser,
      helper,
      Parser )
import Options.Applicative qualified as Opt
import System.Environment (getProgName)

import Graphics.UI.GLFW qualified as GLFW
import Vulkan hiding ( MacOSSurfaceCreateInfoMVK(view)
                     , IOSSurfaceCreateInfoMVK(view)
                     , Display
                     , WaylandSurfaceCreateInfoKHR(display)
                     , DisplayPropertiesKHR(display)
                     )
import Vulkan.Zero
import Vulkan.CStruct.Extends

import TH (makeRioClassy)
import Utils (traverseToSnd, plural)
import Foreign (malloc, nullPtr, Storable (peek))
import Data.Coerce (coerce)
import Data.Foldable (find)
import Control.Applicative (ZipList(ZipList))
import Data.List (nub)
import Control.Monad.Trans.Maybe (runMaybeT)

data AppException
  = GLFWInitError
  | GLFWWindowError
  | GLFWVulkanNotSupportedError
  | VkGenericError Result
  | VkValidationLayersNotSupported (NonEmpty ByteString)
  | VkNoPhysicalDevicesError
  | VkNoSuitableDevicesError

instance Exception AppException

instance Show AppException where
  show = T.unpack . textDisplay

instance Display AppException where
  display = \case
    GLFWInitError -> "Failed to initialize GLFW."
    GLFWWindowError -> "Failed to create window."
    GLFWVulkanNotSupportedError -> "GLFW failed to find Vulkan loader or ICD."
    VkGenericError r -> "Vulkan returned error: " <> displayShow r
    VkValidationLayersNotSupported ls ->
      "Requested validation layer" <> num <> " not supported: " <>
        (displayBytesUtf8 . B.intercalate ", " . toList) ls <> "."
      where num = case ls of
              _ :| [] -> " is"
              _       -> "s are"
    VkNoPhysicalDevicesError -> "No physical devices found."
    VkNoSuitableDevicesError -> "No suitable devices found."

data Options = MkOptions { optWidth            :: Natural
                         , optHeight           :: Natural
                         , optVerbose          :: Bool
                         , optValidationLayers :: Bool
                         }

-- To keep track of whether GLFW is initialized
data GLFWToken s = UnsafeMkGLFWToken

data WindowSize = MkWindowSize { windowWidth  :: Natural
                               , windowHeight :: Natural
                               }
makeRioClassy ''WindowSize

data Config = MkConfig { windowSize             :: WindowSize
                       , enableValidationLayers :: Bool
                       }
makeRioClassy ''Config

data App = MkApp { logFunc :: LogFunc
                 , config  :: Config
                 }
makeRioClassy ''App

data QueueFamilyIndices = MkQueueFamilyIndices { graphicsQueueFamily :: Word32
                                               , presentQueueFamily  :: Word32
                                               }
makeRioClassy ''QueueFamilyIndices

data Queues = MkQueues { graphicsQueue :: Queue
                       , presentQueue  :: Queue
                       }
makeRioClassy ''Queues

data SwapChainSupportDetails = MkSwapChainSupportDetails { swapChainCapabilities :: SurfaceCapabilitiesKHR
                                                         , swapChainFormats      :: NonEmpty SurfaceFormatKHR
                                                         , swapChainPresentModes :: NonEmpty PresentModeKHR
                                                         }
makeRioClassy ''SwapChainSupportDetails

data VulkanApp = MkVulkanApp { app                     :: App
                             , window                  :: GLFW.Window
                             , inst                    :: Instance
                             , device                  :: Device
                             , queueFamilyIndices      :: QueueFamilyIndices
                             , queues                  :: Queues
                             , surface                 :: SurfaceKHR
                             , swapChainSupportDetails :: SwapChainSupportDetails
                             }
makeRioClassy ''VulkanApp

mkConfig :: Options -> Config
mkConfig (MkOptions w h _ l) = MkConfig (MkWindowSize w h) l

options :: Parser Options
options = MkOptions
  <$> option auto
      ( long "width"
     <> help "The width of the window"
     <> showDefault
     <> Opt.value 800
     <> metavar "WIDTH")
  <*> option auto
      ( long "height"
     <> help "The height of the window"
     <> showDefault
     <> Opt.value 600
     <> metavar "HEIGHT")
  <*> switch
      ( long "verbose"
     <> short 'v'
     <> help "Print verbose log messages")
  <*> switch
      ( long "val-layers"
     <> short 'l'
     <> help "Enable Vulkan validation layers")

main :: IO ()
main = do
  opts <- execParser (Opt.info (options <**> helper) fullDesc)
  logOptions <- logOptionsHandle stderr (optVerbose opts)
  withLogFunc logOptions \logFunc -> do
    let app = MkApp { logFunc = logFunc
                    , config = mkConfig opts
                    }
    runRIO app $ catch runApp \e -> do
      logError $ display @SomeException e
      exitFailure

withGLFW :: (forall s . GLFWToken s -> RIO env a) -> RIO env a
withGLFW action = bracket
  do liftIO GLFW.init
  do const $ liftIO GLFW.terminate
  \success -> if | success   -> action UnsafeMkGLFWToken
                 | otherwise -> throwIO GLFWInitError

withWindow :: HasWindowSize env => GLFWToken s -> (GLFW.Window -> RIO env a) -> RIO env a
withWindow _ action = do
  width <- views windowWidthL fromIntegral
  height <- views windowHeightL fromIntegral
  bracket
    do liftIO do
         mapM_ @[] GLFW.windowHint [ GLFW.WindowHint'ClientAPI GLFW.ClientAPI'NoAPI
                                   , GLFW.WindowHint'Resizable False
                                   ]
         progName <- getProgName
         GLFW.createWindow width height progName Nothing Nothing
    do liftIO . mapM_ GLFW.destroyWindow
    do maybe (throwIO GLFWWindowError) action

withWindowSurface :: Instance -> GLFW.Window -> (SurfaceKHR -> RIO env a) -> RIO env a
withWindowSurface inst window = bracket
  do liftIO do surfacePtr <- malloc @SurfaceKHR
               result <- GLFW.createWindowSurface (instanceHandle inst) window nullPtr surfacePtr
               peek =<< catchVk (pure (coerce @Int32 result, surfacePtr))
  do destroySurfaceKHR inst ?? Nothing

mainLoop :: HasVulkanApp env => RIO env ()
mainLoop = whileM do
  liftIO GLFW.waitEvents -- XXX might need pollEvents instead
  liftIO . fmap not . GLFW.windowShouldClose =<< view windowL

validationLayers :: HasEnableValidationLayers env => RIO env (Vector ByteString)
validationLayers = fmap V.fromList $ view enableValidationLayersL >>= bool (pure []) do
   properties <- catchVk enumerateInstanceLayerProperties
   case NE.nonEmpty $ filter (not . (properties `hasLayer`)) layers of
     Nothing          -> pure layers
     Just unsupported -> throwIO $ VkValidationLayersNotSupported unsupported
  where
    hasLayer props = V.elem ?? V.map layerName props
    layers = ["VK_LAYER_KHRONOS_validation"]

pickPhysicalDevice :: HasLogFunc env
                   => Instance -> SurfaceKHR
                   -> RIO env (PhysicalDevice, SwapChainSupportDetails, QueueFamilyIndices)
pickPhysicalDevice inst surface = do
  catchVk (enumeratePhysicalDevices inst) >>= do
    toList >>> NE.nonEmpty >>> maybe
      do throwIO VkNoPhysicalDevicesError
      \physDevs -> do
        let lds = length physDevs
        logDebug $ "Found " <> display lds <> plural " physical device" lds <> "."

        extCandidates <- filterM checkDeviceExtensionSupport $ toList physDevs
        let les = length extCandidates
        logDebug $ "Found " <> display les <> plural " physical device" les <>
          " supporting the extensions " <> displayShow deviceExtensions <> "."

        scCandidates <- mapMaybeM (fmap sequence . traverseToSnd querySwapChainSupport) extCandidates
        let lss = length scCandidates
        logDebug $ "Found " <> display lss <> plural " physical device" lss <>
          " with sufficient swap chain support."

        forMaybeM scCandidates (\(d, sc) -> runMaybeT do
          Just qf <- findQueueFamilies d
          pure (d, sc, qf)) <&> NE.nonEmpty >>= maybe
            do throwIO VkNoSuitableDevicesError
            do (fst . NE.maximumOn1 snd <$>) . mapM (traverseToSnd $ score . view _1)
  where
    score :: PhysicalDevice -> RIO env Integer
    score = (bool 1000 0 . (PHYSICAL_DEVICE_TYPE_DISCRETE_GPU ==) . deviceType <$>) .
      getPhysicalDeviceProperties

    findQueueFamilies :: (MonadReader env m, MonadIO m, HasLogFunc env)
                      => PhysicalDevice -> m (Maybe QueueFamilyIndices)
    findQueueFamilies physDev = do
      props <- getPhysicalDeviceQueueFamilyProperties physDev
      let graphics = fromIntegral <$> V.findIndex (\p -> queueFlags p .&. QUEUE_GRAPHICS_BIT > zero) props
      logResult "graphics" graphics

      let indices = ZipList (toList props) *> [0..]
          surfaceSupport = getPhysicalDeviceSurfaceSupportKHR physDev ?? surface
      present <- fmap fst . find snd <$> (traverse . traverseToSnd) surfaceSupport indices
      logResult "present" present

      pure $ MkQueueFamilyIndices <$> graphics <*> present

    logResult name = maybe do logDebug $ "Couldn't find " <> name <> " queue family on candidate device."
                           \i -> logDebug $ "Found " <> name <> " queue family (index " <> display i <> ")."

    checkDeviceExtensionSupport :: PhysicalDevice -> RIO env Bool
    checkDeviceExtensionSupport physDev = do
      exts <- fmap extensionName <$> catchVk (enumerateDeviceExtensionProperties physDev Nothing)
      pure $ V.all (`elem` exts) deviceExtensions

    querySwapChainSupport :: PhysicalDevice -> RIO env (Maybe SwapChainSupportDetails)
    querySwapChainSupport physDev = runMaybeT do
      let formats      = getPhysicalDeviceSurfaceFormatsKHR physDev surface
          presentModes = getPhysicalDeviceSurfacePresentModesKHR physDev surface
      swapChainCapabilities      <- getPhysicalDeviceSurfaceCapabilitiesKHR physDev surface
      Just swapChainFormats      <- NE.nonEmpty . toList <$> catchVk formats
      Just swapChainPresentModes <- NE.nonEmpty . toList <$> catchVk presentModes
      pure $ MkSwapChainSupportDetails{..}

deviceExtensions :: Vector ByteString
deviceExtensions =
  [ KHR_SWAPCHAIN_EXTENSION_NAME
  ]

catchVk :: MonadIO m => m (Result, a) -> m a
catchVk = (=<<) \case
  (SUCCESS, x) -> pure x
  (err    , _) -> throwIO $ VkGenericError err

runApp :: HasApp env => RIO env ()
runApp = do
  logDebug "Started boxticle"

  withGLFW \glfwToken -> do
    logDebug "Initialized GLFW."

    unlessM (liftIO GLFW.vulkanSupported) $ throwIO GLFWVulkanNotSupportedError
    logDebug "Verified GLFW Vulkan support."

    enabledExtensionNames <- V.fromList <$>
      do liftIO GLFW.getRequiredInstanceExtensions >>= liftIO . mapM B.packCString
    logDebug $ "Required extensions for GLFW: " <> displayShow enabledExtensionNames

    enabledLayerNames <- validationLayers
    let applicationInfo = Just (zero{apiVersion = MAKE_API_VERSION 1 0 0} :: ApplicationInfo)
        instanceCreateInfo = zero{applicationInfo, enabledExtensionNames, enabledLayerNames}

    withInstance instanceCreateInfo Nothing bracket \inst -> do
      withWindow glfwToken \window -> do
        logDebug "Created window."

        withWindowSurface inst window \surface -> do
          (physDev, swapChainSupportDetails, queueFamilyIndices@(MkQueueFamilyIndices{..})) <-
            pickPhysicalDevice inst surface
          logDebug "Picked device."

          let queueCreateInfos = V.fromList (nub [graphicsQueueFamily, presentQueueFamily]) <&>
                \index -> SomeStruct zero{queueFamilyIndex = index, queuePriorities = [1]}
              deviceCreateInfo = zero{ queueCreateInfos
                                     , enabledLayerNames
                                     , enabledExtensionNames = deviceExtensions
                                     }
          withDevice physDev deviceCreateInfo Nothing bracket \device -> do
            logDebug "Created logical device."

            queues <- uncurry MkQueues <$>
              join bitraverse (getDeviceQueue device ?? 0) (graphicsQueueFamily, presentQueueFamily)
            logDebug "Obtained device queues."

            mapRIO (\env -> MkVulkanApp {app = env^.appL, ..}) (logDebug "Starting main loop." *> mainLoop)

  logInfo "Goodbye!"
