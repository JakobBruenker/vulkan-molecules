{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}

module Main where

import RIO
import RIO.ByteString qualified as B
import RIO.NonEmpty qualified as NE
import RIO.Text qualified as T
import RIO.Vector qualified as V
import Data.List.NonEmpty.Extra qualified as NE

import Control.Lens (views, (??), _1, both)
import Control.Monad.Extra (whileM, fromMaybeM, ifM)
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
import Utils (traverseToSnd, plural, clamp, bracketCont)
import Foreign (malloc, nullPtr, Storable (peek))
import Data.Coerce (coerce)
import Data.Foldable (find)
import Control.Applicative (ZipList(..))
import Data.List (nub)
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad.Trans.Cont (evalContT, ContT(..))

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
data GLFWToken = UnsafeMkGLFWToken

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

data Queues = MkQueues { graphicsQueue :: Queue
                       , presentQueue  :: Queue
                       }
makeRioClassy ''Queues

data SwapchainDetails = MkSwapchainDetails { swapchainFormat :: SurfaceFormatKHR
                                           , swapchainExtent :: Extent2D
                                           , swapchainImages :: Vector Image
                                           }
makeRioClassy ''SwapchainDetails

data GraphicsResources = MkGraphicsResources { window           :: GLFW.Window
                                             , inst             :: Instance
                                             , device           :: Device
                                             , queues           :: Queues
                                             , surface          :: SurfaceKHR
                                             , swapchainDetails :: SwapchainDetails
                                             }
makeRioClassy ''GraphicsResources

data VulkanApp = MkVulkanApp { app               :: App
                             , graphicsResources :: GraphicsResources
                             }
makeRioClassy ''VulkanApp

data QueueFamilyIndices = MkQueueFamilyIndices { graphicsQueueFamily :: Word32
                                               , presentQueueFamily  :: Word32
                                               }

data SwapchainSupportDetails = MkSwapchainSupportDetails { swapchainCapabilities :: SurfaceCapabilitiesKHR
                                                         , swapchainFormats      :: NonEmpty SurfaceFormatKHR
                                                         , swapchainPresentModes :: NonEmpty PresentModeKHR
                                                         }

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
main = evalContT do
  opts <- lift $ execParser (Opt.info (options <**> helper) fullDesc)
  logOptions <- logOptionsHandle stderr (optVerbose opts)
  logFunc <- ContT $ withLogFunc logOptions
  runRIO (MkApp {logFunc, config = mkConfig opts}) $ catch runApp \e -> do
    logError $ display @SomeException e
    exitFailure

withGLFW :: MonadUnliftIO m => ContT r m GLFWToken
withGLFW = bracketCont
  do liftIO $ ifM GLFW.init (pure UnsafeMkGLFWToken)
                            (throwIO GLFWInitError)
  do const $ liftIO GLFW.terminate

withWindow :: (MonadUnliftIO m, MonadReader env m, HasWindowSize env)
           => GLFWToken -> ContT r m GLFW.Window
withWindow !_ = do
  width <- views windowWidthL fromIntegral
  height <- views windowHeightL fromIntegral
  bracketCont
    do liftIO do
         mapM_ @[] GLFW.windowHint [ GLFW.WindowHint'ClientAPI GLFW.ClientAPI'NoAPI
                                   , GLFW.WindowHint'Resizable False
                                   ]
         progName <- getProgName
         fromMaybeM (throwIO GLFWWindowError) $
           GLFW.createWindow width height progName Nothing Nothing
    do liftIO . GLFW.destroyWindow

withWindowSurface :: MonadUnliftIO m => Instance -> GLFW.Window -> ContT r m SurfaceKHR
withWindowSurface inst window = bracketCont
  do liftIO do surfacePtr <- malloc @SurfaceKHR
               result <- GLFW.createWindowSurface (instanceHandle inst) window nullPtr surfacePtr
               peek =<< catchVk (pure (coerce @Int32 result, surfacePtr))
  do destroySurfaceKHR inst ?? Nothing

mainLoop :: HasVulkanApp env => RIO env ()
mainLoop = do
  logDebug "Starting main loop."
  whileM do
    liftIO GLFW.waitEvents -- XXX might need pollEvents instead
    liftIO . fmap not . GLFW.windowShouldClose =<< view windowL

validationLayers :: (MonadIO m, MonadReader env m, HasEnableValidationLayers env)
                 => m (Vector ByteString)
validationLayers = fmap V.fromList $ view enableValidationLayersL >>= bool (pure []) do
   properties <- catchVk enumerateInstanceLayerProperties
   case NE.nonEmpty $ filter (not . (properties `hasLayer`)) layers of
     Nothing          -> pure layers
     Just unsupported -> throwIO $ VkValidationLayersNotSupported unsupported
  where
    hasLayer props = V.elem ?? V.map layerName props
    layers = ["VK_LAYER_KHRONOS_validation"]

pickPhysicalDevice :: (MonadIO m, MonadReader env m, HasLogFunc env)
                   => Instance -> SurfaceKHR
                   -> m (PhysicalDevice, SwapchainSupportDetails, QueueFamilyIndices)
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

        scCandidates <- mapMaybeM (fmap sequence . traverseToSnd querySwapchainSupport) extCandidates
        let lss = length scCandidates
        logDebug $ "Found " <> display lss <> plural " physical device" lss <>
          " with sufficient swap chain support."

        forMaybeM scCandidates (\(d, sc) -> runMaybeT do
          Just qf <- findQueueFamilies d
          pure (d, sc, qf)) <&> NE.nonEmpty >>= maybe
            do throwIO VkNoSuitableDevicesError
            do (fst . NE.maximumOn1 snd <$>) . mapM (traverseToSnd $ score . view _1)
  where
    score :: MonadIO m => PhysicalDevice -> m Integer
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

    logResult name = maybe
      do logDebug $ "Couldn't find " <> name <> " queue family on candidate device."
      \i -> logDebug $ "Found " <> name <> " queue family (index " <> display i <> ")."

    checkDeviceExtensionSupport :: MonadIO m => PhysicalDevice -> m Bool
    checkDeviceExtensionSupport physDev = do
      exts <- fmap extensionName <$> catchVk (enumerateDeviceExtensionProperties physDev Nothing)
      pure $ V.all (`elem` exts) deviceExtensions

    querySwapchainSupport :: MonadIO m => PhysicalDevice -> m (Maybe SwapchainSupportDetails)
    querySwapchainSupport physDev = runMaybeT do
      let formats      = getPhysicalDeviceSurfaceFormatsKHR physDev surface
          presentModes = getPhysicalDeviceSurfacePresentModesKHR physDev surface
      swapchainCapabilities      <- getPhysicalDeviceSurfaceCapabilitiesKHR physDev surface
      Just swapchainFormats      <- NE.nonEmpty . toList <$> catchVk formats
      Just swapchainPresentModes <- NE.nonEmpty . toList <$> catchVk presentModes
      pure $ MkSwapchainSupportDetails{..}

deviceExtensions :: Vector ByteString
deviceExtensions =
  [ KHR_SWAPCHAIN_EXTENSION_NAME

  ]

swapchainCreateInfo :: MonadIO m
                    => GLFW.Window -> SurfaceKHR -> SwapchainSupportDetails -> QueueFamilyIndices
                    -> m (SwapchainCreateInfoKHR '[])
swapchainCreateInfo window
                    surface
                    (MkSwapchainSupportDetails capabilities formats _)
                    (MkQueueFamilyIndices{graphicsQueueFamily, presentQueueFamily}) = do
  imageExtent <- liftIO extent
  pure zero{ surface
           , minImageCount = imageCount
           , imageFormat
           , imageColorSpace
           , imageExtent
           , imageArrayLayers = 1
           , imageUsage = IMAGE_USAGE_COLOR_ATTACHMENT_BIT
           , imageSharingMode
           , queueFamilyIndices
           , preTransform = currentTransform
           , compositeAlpha = COMPOSITE_ALPHA_OPAQUE_BIT_KHR
           , presentMode
           , clipped = True
           }
  where
    imageSharingMode | graphicsQueueFamily == presentQueueFamily = SHARING_MODE_EXCLUSIVE
                     | otherwise                                 = SHARING_MODE_CONCURRENT
    queueFamilyIndices = V.fromList $ nub [graphicsQueueFamily, presentQueueFamily]
    imageCount = clamp (minImageCount, bool maxBound maxImageCount $ maxImageCount > 0) desired
      where desired = minImageCount + 1
    SurfaceFormatKHR{format = imageFormat, colorSpace = imageColorSpace} = liftA2 fromMaybe NE.head
            (find (== SurfaceFormatKHR FORMAT_R8G8B8A8_SRGB COLOR_SPACE_SRGB_NONLINEAR_KHR))
            formats
    presentMode = PRESENT_MODE_FIFO_KHR
    extent | currentWidth /= maxBound = pure currentExtent
           | otherwise = do
             (fbWidth, fbHeight) <- over both fromIntegral <$> GLFW.getFramebufferSize window
             let width = clamp (minWidth, maxWidth) fbWidth
                 height = clamp (minHeight, maxHeight) fbHeight
             pure Extent2D{width, height}
      where Extent2D{width = currentWidth} = currentExtent
            Extent2D{width = minWidth, height = minHeight} = minImageExtent
            Extent2D{width = maxWidth, height = maxHeight} = maxImageExtent

    SurfaceCapabilitiesKHR{ currentExtent, currentTransform
                          , minImageExtent, maxImageExtent
                          , minImageCount, maxImageCount
                          } = capabilities

-- TODO this is probably unnecessary, since the library says it already throws
-- an exception if the result is not SUCCESS
catchVk :: MonadIO m => m (Result, a) -> m a
catchVk = (=<<) \case
  (SUCCESS, x) -> pure x
  (err    , _) -> throwIO $ VkGenericError err

acquireGraphicsResources :: (MonadUnliftIO m, MonadReader env m, HasApp env)
                         => ContT r m GraphicsResources
acquireGraphicsResources = do
  glfwToken <- withGLFW
  logDebug "Initialized GLFW."

  unlessM (liftIO GLFW.vulkanSupported) $ throwIO GLFWVulkanNotSupportedError
  logDebug "Verified GLFW Vulkan support."

  enabledExtensionNames <- V.fromList <$>
    do liftIO GLFW.getRequiredInstanceExtensions >>= liftIO . mapM B.packCString
  logDebug $ "Required extensions for GLFW: " <> displayShow enabledExtensionNames

  enabledLayerNames <- validationLayers
  let applicationInfo = Just (zero{apiVersion = MAKE_API_VERSION 1 0 0} :: ApplicationInfo)
      instanceCreateInfo = zero{applicationInfo, enabledExtensionNames, enabledLayerNames}

  inst <- withInstance instanceCreateInfo Nothing bracketCont
  window <- withWindow glfwToken
  logDebug "Created window."

  surface <- withWindowSurface inst window
  logDebug "Created surface."

  (physDev, swapchainSupportDetails, queueFamilyIndices@(MkQueueFamilyIndices{..})) <-
    pickPhysicalDevice inst surface
  logDebug "Picked device."

  let queueCreateInfos = V.fromList (nub [graphicsQueueFamily, presentQueueFamily]) <&>
        \index -> SomeStruct zero{queueFamilyIndex = index, queuePriorities = [1]}
      deviceCreateInfo = zero{ queueCreateInfos
                             , enabledLayerNames
                             , enabledExtensionNames = deviceExtensions
                             }
  device <- withDevice physDev deviceCreateInfo Nothing bracketCont
  logDebug "Created logical device."

  queues <- uncurry MkQueues <$>
    join bitraverse (getDeviceQueue device ?? 0) (graphicsQueueFamily, presentQueueFamily)
  logDebug "Obtained device queues."

  scInfo@(SwapchainCreateInfoKHR {imageFormat, imageColorSpace, imageExtent = swapchainExtent}) <-
    swapchainCreateInfo window surface swapchainSupportDetails queueFamilyIndices
  swapchain <- withSwapchainKHR device scInfo Nothing bracketCont
  logDebug "Created swapchain."

  swapchainImages <- catchVk $ getSwapchainImagesKHR device swapchain
  let swapchainFormat  = SurfaceFormatKHR imageFormat imageColorSpace
      swapchainDetails = MkSwapchainDetails{..}

  pure $ MkGraphicsResources{..}


runApp :: HasApp env => RIO env ()
runApp = evalContT do
  logDebug "Started boxticle."

  graphicsResources <- acquireGraphicsResources
  lift $ mapRIO (\env -> MkVulkanApp {app = env^.appL, ..}) mainLoop
