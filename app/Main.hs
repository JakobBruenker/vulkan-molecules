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
import Control.Monad.Trans.Resource (allocate, ReleaseKey, runResourceT, ResIO, MonadResource)
import Data.Coerce (coerce)
import Data.Finite (Finite, natToFinite, modulo)
import Data.Foldable (find)
import Data.List (nub)
import Data.Tuple.Extra (dupe)
import Foreign (malloc, nullPtr, Storable (peek))

import Data.Vector.Sized qualified as Sized
import Data.Constraint (Dict(..))

import Graphics.UI.GLFW qualified as GLFW
import Vulkan hiding ( MacOSSurfaceCreateInfoMVK(view)
                     , IOSSurfaceCreateInfoMVK(view)
                     , Display
                     , WaylandSurfaceCreateInfoKHR(display)
                     , DisplayPropertiesKHR(display)
                     )
import Vulkan.Zero
import Vulkan.CStruct.Extends

import Internal.Types(GLFWToken(UnsafeMkGLFWToken))

import Utils
import Shaders
import Types
import Swapchain
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

withGLFW :: ResIO (ReleaseKey, GLFWToken)
withGLFW = allocate
  do liftIO $ ifM GLFW.init do pure UnsafeMkGLFWToken
                            do throwIO GLFWInitError
  do const $ liftIO GLFW.terminate

withWindow :: (HasLogger, HasConfig) => GLFWToken -> ResIO (ReleaseKey, GLFW.Window)
withWindow !_ = do
  monitor <- preview (ix $ fromIntegral ?monitorIndex) . concat <$> liftIO GLFW.getMonitors
  when (?fullscreen && isNothing monitor) $
    logWarn "Couldn't find desired monitor. Using windowed mode."
  (xPos, yPos, width, height) <- if
    | ?fullscreen, Just mon <- monitor -> liftIO $ GLFW.getMonitorWorkarea mon
    | otherwise -> pure (0, 0, fromIntegral ?windowWidth, fromIntegral ?windowHeight)

  allocate do traverse_ @[] GLFW.windowHint [ GLFW.WindowHint'ClientAPI GLFW.ClientAPI'NoAPI
                                            , GLFW.WindowHint'Resizable (not ?fullscreen)
                                            , GLFW.WindowHint'Decorated (not ?fullscreen)
                                            , GLFW.WindowHint'Floating ?fullscreen
                                            ]
              progName <- getProgName
              maybeM (throwIO GLFWWindowError)
                     (\window -> GLFW.setWindowPos window xPos yPos $> window)
                     (GLFW.createWindow width height progName Nothing Nothing)
           do GLFW.destroyWindow

withWindowSurface :: (HasInstance, HasWindow) => ResIO (ReleaseKey, SurfaceKHR)
withWindowSurface = allocate
  do liftIO do surfacePtr <- malloc @SurfaceKHR
               result <- GLFW.createWindowSurface (instanceHandle ?instance) ?window nullPtr surfacePtr
               peek =<< catchVk (coerce @Int32 result, surfacePtr)
  do destroySurfaceKHR ?instance ?? Nothing
  where
    catchVk = \case
      (SUCCESS, x) -> pure x
      (err    , _) -> throwIO $ VkGenericError err

drawFrame :: HasVulkanResources => Finite MaxFramesInFlight -> IO ()
drawFrame currentFrame = do
  swapchain <- readRes ?swapchain

  let ixSync :: Sized.Vector MaxFramesInFlight a -> a
      ixSync = view $ Sized.ix currentFrame

  _result <- waitForFences ?device [ixSync ?inFlight] True maxBound

  (_, imageIndex) <-
    acquireNextImageKHR ?device swapchain maxBound (ixSync ?imageAvailable) NULL_HANDLE

  MkImageRelated{commandBuffer, imageInFlight} <- fromMaybeM
    do throwIO VkCommandBufferIndexOutOfRange
    do pure $ ?imageRelateds^?ix (fromIntegral imageIndex)

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

mainLoop :: HasApp => IO ()
mainLoop = do
  logDebug "Starting main loop."
  fix ?? natToFinite (Proxy @0) $ \loop currentFrame -> do
    drawFrame currentFrame
    liftIO GLFW.waitEvents
    let nextFrame = modulo @MaxFramesInFlight $ fromIntegral currentFrame + 1
    unlessM ?? loop nextFrame $ liftIO $ GLFW.windowShouldClose ?window

  -- Allow queues/buffers to finish their job
  deviceWaitIdleSafe ?device

validationLayers :: HasEnableValidationLayers => ResIO (Vector ByteString)
validationLayers = fmap V.fromList $ ?enableValidationLayers & bool (pure []) do
   (_, properties) <- enumerateInstanceLayerProperties
   case NE.nonEmpty $ filter (not . (properties `hasLayer`)) layers of
     Nothing          -> pure layers
     Just unsupported -> throwIO $ VkValidationLayersNotSupported unsupported
  where
    hasLayer props = V.elem ?? V.map layerName props
    layers = ["VK_LAYER_KHRONOS_validation"]

pickPhysicalDevice :: (HasLogger, HasInstance, HasSurface)
                   => ResIO (Dict (HasPhysicalDevice, HasSwapchainSupport, HasQueueFamilyIndices))
pickPhysicalDevice = do
  enumeratePhysicalDevices ?instance >>= do
    snd >>> toList >>> NE.nonEmpty >>> maybe
      do throwIO VkNoPhysicalDevicesError
      \(toList -> physDevs) -> do
        let lds = length physDevs
        logDebug $ "Found " <> display lds <> plural " physical device" lds <> "."

        fmap (fst . maximumOn1 snd) $
          fromMaybeM (throwIO VkNoSuitableDevicesError) . fmap NE.nonEmpty $
            mapMaybeM ?? physDevs $ \physDev -> runMaybeT do
              let ?physicalDevice = physDev
              guard =<< checkDeviceExtensionSupport
              Dict <- querySwapchainSupport
              Dict <- findQueueFamilies
              (Dict,) <$> score
  where
    score :: (MonadIO m, HasPhysicalDevice) => m Integer
    score = (bool 1000 0 . (PHYSICAL_DEVICE_TYPE_DISCRETE_GPU ==) . deviceType <$>) $
      getPhysicalDeviceProperties ?physicalDevice

    findQueueFamilies :: (MonadResource m, HasLogger, HasPhysicalDevice)
                      => MaybeT m (Dict HasQueueFamilyIndices)
    findQueueFamilies = do
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

    logResult name i = logDebug $ "Found " <> name <> " queue family (index " <> display i <> ")."

    checkDeviceExtensionSupport :: (MonadIO m, HasPhysicalDevice) => m Bool
    checkDeviceExtensionSupport = do
      exts <- fmap extensionName . snd <$> enumerateDeviceExtensionProperties ?physicalDevice Nothing
      pure $ V.all (`elem` exts) deviceExtensions

    querySwapchainSupport :: (MonadIO m, HasPhysicalDevice) => MaybeT m (Dict HasSwapchainSupport)
    querySwapchainSupport = do
      let formats      = getPhysicalDeviceSurfaceFormatsKHR ?physicalDevice ?surface
          presentModes = getPhysicalDeviceSurfacePresentModesKHR ?physicalDevice ?surface
      swapchainCapabilities      <- getPhysicalDeviceSurfaceCapabilitiesKHR ?physicalDevice ?surface
      Just swapchainFormats      <- NE.nonEmpty . toList . snd <$> formats
      Just swapchainPresentModes <- NE.nonEmpty . toList . snd <$> presentModes
      let ?swapchainCapabilities = swapchainCapabilities
          ?swapchainFormats      = swapchainFormats
          ?swapchainPresentModes = swapchainPresentModes
      pure Dict

deviceExtensions :: Vector ByteString
deviceExtensions =
  [ KHR_SWAPCHAIN_EXTENSION_NAME
  ]

setupCommands :: (HasLogger, HasVulkanResources) => IO ()
setupCommands = do
  for_ ?imageRelateds \MkImageRelated{commandBuffer, framebuffer = framebufferRes} ->
    useCommandBuffer commandBuffer zero do
      renderPass <- readRes ?renderPass
      extent <- readIORef ?swapchainExtent
      graphicsPipeline <- readRes ?graphicsPipeline
      framebuffer <- readRes framebufferRes
      let renderPassInfo = zero{ renderPass
                               , framebuffer
                               , renderArea = zero{extent}
                               , clearValues = [Color $ Float32 0 0 0 1]
                               }
      cmdUseRenderPass commandBuffer renderPassInfo SUBPASS_CONTENTS_INLINE do
        cmdBindPipeline commandBuffer PIPELINE_BIND_POINT_GRAPHICS graphicsPipeline
        cmdDraw commandBuffer 3 1 0 0
  logDebug "Set up commands."

constructImageRelateds :: (HasDevice, HasSwapchainRelated, HasCommandPool)
                       => ResIO (Vector ImageRelated)
constructImageRelateds = do
  swapchain <- readRes ?swapchain
  (_, images) <- getSwapchainImagesKHR ?device swapchain
  commandBuffers <- (constructCommandBuffers . fromIntegral . length) images
  V.zipWithM constructImageRelated images commandBuffers

constructCommandBuffers :: (HasDevice, HasCommandPool) => Natural -> ResIO (Vector CommandBuffer)
constructCommandBuffers count = do
  let commandBuffersInfo = zero{ commandPool = ?commandPool
                               , level = COMMAND_BUFFER_LEVEL_PRIMARY
                               , commandBufferCount = fromIntegral count
                               }
  snd <$> withCommandBuffers ?device commandBuffersInfo allocate

constructImageRelated :: (HasDevice, HasSwapchainRelated)
                      => Image -> CommandBuffer -> ResIO ImageRelated
constructImageRelated img commandBuffer = do
  image          <- newIORef img
  imageInFlight  <- constructImageInFlight
  imageView      <- constructImageView img
  framebuffer    <- constructFramebuffer =<< readRes imageView
  pure MkImageRelated{..}

constructImageInFlight :: MonadIO m => m (IORef (Maybe Fence))
constructImageInFlight = newIORef Nothing

constructImageView :: (HasDevice, HasSwapchainRelated) => Image -> ResIO (MResource ImageView)
constructImageView image = do
  let ivInfo = zero{ viewType = IMAGE_VIEW_TYPE_2D
                   , format = ?swapchainFormat^.formatL
                   , subresourceRange
                   }
      subresourceRange = zero{ aspectMask = IMAGE_ASPECT_COLOR_BIT
                             , levelCount = 1
                             , layerCount = 1
                             }

  mkMResource =<< withImageView ?device ivInfo{image} Nothing allocate

constructFramebuffer :: (HasDevice, HasSwapchainRelated) => ImageView -> ResIO (MResource Framebuffer)
constructFramebuffer imageView = do
  renderPass <- readRes ?renderPass
  Extent2D{width, height} <- readIORef ?swapchainExtent
  let fbInfo = zero{ renderPass
                   , width
                   , height
                   , layers = 1
                   }
  mkMResource =<< withFramebuffer ?device fbInfo{attachments = [imageView]} Nothing allocate

withGraphicsResources :: (HasLogger, HasConfig) => ResIO (Dict HasGraphicsResources)
withGraphicsResources = do
  (_, glfwToken) <- withGLFW
  logDebug "Initialized GLFW."

  unlessM (liftIO GLFW.vulkanSupported) $ throwIO GLFWVulkanNotSupportedError
  logDebug "Verified GLFW Vulkan support."

  enabledExtensionNames <- V.fromList <$>
    do liftIO GLFW.getRequiredInstanceExtensions >>= liftIO . mapM B.packCString
  logDebug $ "Required extensions for GLFW: " <> displayShow enabledExtensionNames

  enabledLayerNames <- validationLayers
  let applicationInfo = Just (zero{apiVersion = MAKE_API_VERSION 1 0 0} :: ApplicationInfo)
      instanceCreateInfo = zero{applicationInfo, enabledExtensionNames, enabledLayerNames}

  (_, inst  ) <- withInstance instanceCreateInfo Nothing allocate
  (_, window) <- withWindow glfwToken
  let ?instance = inst
      ?window   = window
  logDebug "Created window."

  (_, surface) <- withWindowSurface
  let ?surface = surface
  logDebug "Created surface."

  Dict <- pickPhysicalDevice
  logDebug "Picked device."

  let queueCreateInfos = V.fromList (nub [?graphicsQueueFamily, ?presentQueueFamily]) <&>
        \index -> SomeStruct zero{queueFamilyIndex = index, queuePriorities = [1]}
      deviceCreateInfo = zero{ queueCreateInfos
                             , enabledLayerNames
                             , enabledExtensionNames = deviceExtensions
                             }
  (_, device) <- withDevice ?physicalDevice deviceCreateInfo Nothing allocate
  let ?device = device
  logDebug "Created logical device."

  (graphicsQueue, presentQueue) <-
    join bitraverse (getDeviceQueue device ?? 0) (?graphicsQueueFamily, ?presentQueueFamily)
  let ?graphicsQueue = graphicsQueue
      ?presentQueue  = presentQueue
  logDebug "Obtained device queues."

  pure Dict

runApp :: (HasLogger, HasConfig) => IO ()
runApp = runResourceT do
  liftIO compileAllShaders
  logDebug "Compiled shaders."

  logDebug "Started boxticle."

  Dict <- withGraphicsResources

  Dict <- withSwapchain

  let commandPoolInfo = zero{ queueFamilyIndex = ?graphicsQueueFamily
                            } :: CommandPoolCreateInfo
  (_, commandPool) <- withCommandPool ?device commandPoolInfo Nothing allocate
  let ?commandPool = commandPool
  logDebug "Created command pool."

  imageRelateds <- constructImageRelateds
  let ?imageRelateds = imageRelateds
  logDebug "Created buffers."

  (imageAvailable, renderFinished) <- bisequence . dupe . Sized.replicateM $
    snd <$> withSemaphore ?device zero Nothing allocate
  inFlight <- Sized.replicateM $
    snd <$> withFence ?device zero{flags = FENCE_CREATE_SIGNALED_BIT} Nothing allocate
  let ?imageAvailable = imageAvailable
      ?renderFinished = renderFinished
      ?inFlight       = inFlight
  logDebug "Created syncs."

  lift $ setupCommands *> mainLoop

  logInfo "Goodbye!"
