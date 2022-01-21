{-# LANGUAGE OverloadedLists #-}

module Main (main) where

import RIO hiding (logDebug, logInfo, logWarn, logError)
import RIO.ByteString qualified as B
import RIO.NonEmpty qualified as NE
import RIO.Vector qualified as V

import Control.Lens ((??), ix)
import Control.Monad.Extra (fromMaybeM, ifM, whenJust, maybeM)
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
import Control.Applicative (ZipList(..))
import Control.Monad.Trans.Maybe (runMaybeT, MaybeT)
import Control.Monad.Trans.Resource (allocate, runResourceT, ResIO, MonadResource)
import Data.Coerce (coerce)
import Data.Finite (Finite, natToFinite)
import Data.Foldable (find)
import Data.List (nub)
import Foreign (malloc, nullPtr, Storable (peek))

import Data.Vector.Sized qualified as Sized

import Graphics.UI.GLFW qualified as GLFW
import Vulkan hiding ( MacOSSurfaceCreateInfoMVK(view)
                     , IOSSurfaceCreateInfoMVK(view)
                     , Display
                     , WaylandSurfaceCreateInfoKHR(display)
                     , DisplayPropertiesKHR(display)
                     )
import Vulkan.Zero
import Vulkan.CStruct.Extends
import Vulkan.Exception

import Internal.Types(GLFWToken(UnsafeMkGLFWToken))

import Utils
import Shaders
import Types
import Mutables
import Data.List.NonEmpty.Extra (maximumOn1)

mkConfig :: Options -> Dict HasConfig
mkConfig (MkOptions{..}) = Dict
  where ?windowWidth            = optWidth
        ?windowHeight           = optHeight
        ?fullscreen             = optFullscreen
        ?monitorIndex           = optMonitorIndex
        ?enableValidationLayers = optValidationLayers

options :: Parser Options
options = MkOptions
  <$> option auto
      ( long "width"
     <> help "The width of the window in windowed mode"
     <> showDefault
     <> Opt.value 800
     <> metavar "WIDTH")
  <*> option auto
      ( long "height"
     <> help "The height of the window in windowed mode"
     <> showDefault
     <> Opt.value 600
     <> metavar "HEIGHT")
  <*> switch
      ( long "fullscreen"
     <> short 'f'
     <> help "Start in fullscreen (borderless windowed) mode")
  <*> option auto
      ( long "monitor"
     <> short 'm'
     <> help "Which monitor to use for fullscreen mode"
     <> showDefault
     <> Opt.value 0
     <> metavar "MONITOR_INDEX")
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
    let ?logFunc = logFunc
    Dict <- pure $ mkConfig opts
    catch runApp \e -> do
      logError $ display @SomeException e
      exitFailure

initGLFW :: HasLogger => ResIO (Dict HasGLFW)
initGLFW = do
  (_, glfwToken) <- allocate
    do liftIO $ ifM GLFW.init do pure UnsafeMkGLFWToken
                              do throwIO GLFWInitError
    do const $ liftIO GLFW.terminate
  let ?glfw = glfwToken

  logDebug "Initialized GLFW."
  pure Dict

initInstance :: (HasLogger, HasValidationLayers) => ResIO (Dict HasInstance)
initInstance = do
  unlessM (liftIO GLFW.vulkanSupported) $ throwIO GLFWVulkanNotSupportedError
  logDebug "Verified GLFW Vulkan support."

  enabledExtensionNames <- V.fromList <$>
    do liftIO GLFW.getRequiredInstanceExtensions >>= liftIO . mapM B.packCString
  logDebug $ "Required extensions for GLFW: " <> displayShow enabledExtensionNames

  let applicationInfo = Just (zero{apiVersion = MAKE_API_VERSION 1 0 0} :: ApplicationInfo)
      instanceCreateInfo = zero{ applicationInfo
                               , enabledExtensionNames
                               , enabledLayerNames = ?validationLayers
                               }

  (_, inst) <- withInstance instanceCreateInfo Nothing allocate
  let ?instance = inst

  logDebug "Created instance."
  pure Dict

initWindow :: (HasLogger, HasConfig, HasGLFW) => ResIO (Dict HasWindow)
initWindow = do
  !_ <- pure ?glfw
  monitor <- preview (ix $ fromIntegral ?monitorIndex) . concat <$> liftIO GLFW.getMonitors
  when (?fullscreen && isNothing monitor) $
    logWarn "Couldn't find desired monitor. Using windowed mode."
  let actuallyFullscreen = ?fullscreen && isJust monitor
  (xPos, yPos, width, height) <- if
    | actuallyFullscreen, Just mon <- monitor -> liftIO $ GLFW.getMonitorWorkarea mon
    | otherwise -> pure (0, 0, fromIntegral ?windowWidth, fromIntegral ?windowHeight)

  (_, window) <- allocate
    do traverse_ @[] GLFW.windowHint [ GLFW.WindowHint'ClientAPI GLFW.ClientAPI'NoAPI
                                     , GLFW.WindowHint'Resizable (not actuallyFullscreen)
                                     , GLFW.WindowHint'Decorated (not actuallyFullscreen)
                                     , GLFW.WindowHint'Floating actuallyFullscreen
                                     ]
       progName <- getProgName
       maybeM (throwIO GLFWWindowError)
              (\window -> GLFW.setWindowPos window xPos yPos $> window)
              (GLFW.createWindow width height progName Nothing Nothing)
    do GLFW.destroyWindow
  let ?window = window

  logDebug "Created window."
  pure Dict

initSurface :: (HasLogger, HasInstance, HasWindow) => ResIO (Dict HasSurface)
initSurface = do
  (_, surface) <- allocate
    do liftIO do
         surfacePtr <- malloc @SurfaceKHR
         result <- GLFW.createWindowSurface (instanceHandle ?instance) ?window nullPtr surfacePtr
         peek =<< catchVk (coerce @Int32 result, surfacePtr)
    do destroySurfaceKHR ?instance ?? Nothing
  let ?surface = surface

  logDebug "Created surface."
  pure Dict
  where
    catchVk = \case
      (SUCCESS, x) -> pure x
      (err    , _) -> throwIO $ VulkanException err

drawFrame :: HasVulkanResources => Finite MaxFramesInFlight -> ResIO ()
drawFrame currentFrame = do
  MkMutables{swapchain, imageRelateds} <- readRes ?mutables

  let ixSync :: Sized.Vector MaxFramesInFlight a -> a
      ixSync = view $ Sized.ix currentFrame

  _result <- waitForFences ?device [ixSync ?inFlight] True maxBound

  (_, imageIndex) <-
    acquireNextImageKHR ?device swapchain maxBound (ixSync ?imageAvailable) NULL_HANDLE

  MkImageRelated{commandBuffer, imageInFlight} <- fromMaybeM
    do throwIO VkCommandBufferIndexOutOfRange
    do pure $ imageRelateds^?ix (fromIntegral imageIndex)

  imageInFlightFence <- readIORef imageInFlight

  -- Check if a previous frame is using this image (i.e. there is its fence to wait on)
  whenJust imageInFlightFence \fence -> void $ waitForFences ?device [fence] True maxBound

  -- Mark the image as now being in use by this frame
  writeIORef imageInFlight (Just $ ixSync ?inFlight)

  let submitInfo = pure . SomeStruct $
        SubmitInfo{ next = ()
                  , waitSemaphores = [ixSync ?imageAvailable]
                  , waitDstStageMask = [PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT]
                  , commandBuffers = [commandBufferHandle commandBuffer]
                  , signalSemaphores = [ixSync ?renderFinished]
                  }

  resetFences ?device [ixSync ?inFlight]
  queueSubmit ?graphicsQueue submitInfo (ixSync ?inFlight)

  let presentInfo = PresentInfoKHR{ next = ()
                                  , waitSemaphores = [ixSync ?renderFinished]
                                  , swapchains = [swapchain]
                                  , imageIndices = [imageIndex]
                                  , results = zero
                                  }
  void $ queuePresentKHR ?presentQueue presentInfo

mainLoop :: HasApp => ResIO ()
mainLoop = do
  logDebug "Starting main loop."
  fix ?? natToFinite (Proxy @0) $ \loop currentFrame -> do
    -- NB: depending on present mode and possibly other factors, the exception
    -- is thrown either by 'acquireNextImageKHR' or by 'queuePresentKHR'
    drawFrame currentFrame `catch` \case
      VulkanException ERROR_OUT_OF_DATE_KHR -> recreateSwapchain
      ex -> throwIO ex
    liftIO GLFW.waitEvents
    unlessM ?? loop (currentFrame + 1) $ liftIO $ GLFW.windowShouldClose ?window

  -- Allow queues/buffers to finish their job
  deviceWaitIdleSafe ?device

initValidationLayers :: HasEnableValidationLayers => ResIO (Dict HasValidationLayers)
initValidationLayers = do
  validationLayers <- fmap V.fromList $ ?enableValidationLayers & bool (pure []) do
    (_, properties) <- enumerateInstanceLayerProperties
    case NE.nonEmpty $ filter (not . (properties `hasLayer`)) layers of
      Nothing          -> pure layers
      Just unsupported -> throwIO $ VkValidationLayersNotSupported unsupported
  let ?validationLayers = validationLayers
  pure Dict
  where
    hasLayer props = V.elem ?? V.map layerName props
    layers = ["VK_LAYER_KHRONOS_validation"]

initPhysicalDevice :: (HasLogger, HasInstance, HasSurface)
                   => ResIO (Dict HasPhysicalDeviceRelated)
initPhysicalDevice = do
  dict <- enumeratePhysicalDevices ?instance >>= do
    snd >>> toList >>> NE.nonEmpty >>> maybe
      do throwIO VkNoPhysicalDevicesError
      \(toList -> physDevs) -> do
        let lds = length physDevs
        logDebug $ "Found " <> display lds <> plural " physical device" lds <> "."

        fmap (fst . maximumOn1 snd) $
          fromMaybeM (throwIO VkNoSuitableDevicesError) . fmap NE.nonEmpty $
            mapMaybeM ?? zip [0..] physDevs $ \(index, physDev) -> runMaybeT do
              let ?physicalDevice = physDev
              guard =<< checkDeviceExtensionSupport index
              Dict <- querySwapchainSupport
              Dict <- findQueueFamilies index
              (Dict,) <$> score

  logDebug "Picked device."
  pure dict
  where
    score :: (MonadIO m, HasPhysicalDevice) => m Integer
    score = (bool 1000 0 . (PHYSICAL_DEVICE_TYPE_DISCRETE_GPU ==) . deviceType <$>) $
      getPhysicalDeviceProperties ?physicalDevice

    findQueueFamilies :: (MonadResource m, HasLogger, HasPhysicalDevice)
                      => Natural -> MaybeT m (Dict HasQueueFamilyIndices)
    findQueueFamilies index = do
      props <- getPhysicalDeviceQueueFamilyProperties ?physicalDevice
      Just graphics <- pure $ fromIntegral <$>
        V.findIndex (\p -> queueFlags p .&. QUEUE_GRAPHICS_BIT > zero) props
      logResult "graphics" graphics

      let indices = ZipList (toList props) *> [0..]
          surfaceSupport = getPhysicalDeviceSurfaceSupportKHR ?physicalDevice ?? ?surface
      Just present <- fmap fst . find snd <$> (traverse . traverseToSnd) surfaceSupport indices
      logResult "present" present

      let ?graphicsQueueFamily = graphics
          ?presentQueueFamily  = present
      pure Dict
      where
        logResult name queueIndex = logDebug $
          "Found " <> name <> " queue family (index " <> display queueIndex <> ") on device " <>
          displayShow index <> "."

    checkDeviceExtensionSupport :: (MonadIO m, HasPhysicalDevice) => Natural -> m Bool
    checkDeviceExtensionSupport index = do
      exts <- fmap extensionName . snd <$> enumerateDeviceExtensionProperties ?physicalDevice Nothing
      let yes = V.all (`elem` exts) deviceExtensions
      logDebug $ "Device " <> displayShow index <> " " <> bool "doesn't support" "supports" yes <>
                 " extensions " <> displayShow deviceExtensions <> "."
      pure yes

    querySwapchainSupport :: (MonadIO m, HasPhysicalDevice)
                          => MaybeT m (Dict HasSwapchainSupport)
    querySwapchainSupport = do
      let formats      = getPhysicalDeviceSurfaceFormatsKHR ?physicalDevice ?surface
          presentModes = getPhysicalDeviceSurfacePresentModesKHR ?physicalDevice ?surface
      Just swapchainFormats      <- NE.nonEmpty . toList . snd <$> formats
      Just swapchainPresentModes <- NE.nonEmpty . toList . snd <$> presentModes
      let ?swapchainFormats      = swapchainFormats
          ?swapchainPresentModes = swapchainPresentModes
      pure Dict

initDevice :: (HasLogger, HasPhysicalDeviceRelated, HasValidationLayers)
           => ResIO (Dict HasDevice)
initDevice = do
  let queueCreateInfos = V.fromList (nub [?graphicsQueueFamily, ?presentQueueFamily]) <&>
        \index -> SomeStruct zero{queueFamilyIndex = index, queuePriorities = [1]}
      deviceCreateInfo = zero{ queueCreateInfos
                             , enabledLayerNames = ?validationLayers
                             , enabledExtensionNames = deviceExtensions
                             }
  (_, device) <- withDevice ?physicalDevice deviceCreateInfo Nothing allocate
  let ?device = device

  logDebug "Created logical device."
  pure Dict

initQueues :: (HasLogger, HasDevice, HasQueueFamilyIndices) => ResIO (Dict HasQueues)
initQueues = do
  (graphicsQueue, presentQueue) <-
    join bitraverse (getDeviceQueue ?device ?? 0) (?graphicsQueueFamily, ?presentQueueFamily)
  let ?graphicsQueue = graphicsQueue
      ?presentQueue  = presentQueue

  logDebug "Obtained device queues."
  pure Dict

deviceExtensions :: Vector ByteString
deviceExtensions =
  [ KHR_SWAPCHAIN_EXTENSION_NAME
  ]

initCommandPool :: (HasLogger, HasDevice, HasGraphicsQueueFamily) => ResIO (Dict HasCommandPool)
initCommandPool = do
  let commandPoolInfo = zero{ queueFamilyIndex = ?graphicsQueueFamily
                            } :: CommandPoolCreateInfo
  (_, commandPool) <- withCommandPool ?device commandPoolInfo Nothing allocate
  let ?commandPool = commandPool

  logDebug "Created command pool."
  pure Dict

initializeVulkan :: (HasLogger, HasConfig) => ResIO (Dict HasVulkanResources)
initializeVulkan = do
  Dict <- initGLFW
  Dict <- initValidationLayers
  Dict <- initWindow
  Dict <- initInstance
  Dict <- initSurface
  Dict <- initPhysicalDevice
  Dict <- initDevice
  Dict <- initQueues
  Dict <- initCommandPool
  Dict <- initMutables
  Dict <- initSyncs
  pure Dict

runApp :: (HasLogger, HasConfig) => IO ()
runApp = runResourceT do
  liftIO compileAllShaders
  logDebug "Compiled shaders."

  Dict <- initializeVulkan

  setupCommands *> mainLoop

  logInfo "Goodbye!"
