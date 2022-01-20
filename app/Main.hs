{-# LANGUAGE OverloadedLists #-}

module Main where

import RIO
import RIO.ByteString qualified as B
import RIO.NonEmpty qualified as NE
import RIO.Vector qualified as V
import Data.List.NonEmpty.Extra qualified as NE

import Control.Lens (views, (??), _1, ix)
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
import Control.Monad.Trans.Cont (evalContT, ContT(..))
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad.Trans.Resource (allocate, ReleaseKey, MonadResource, runResourceT)
import Data.Coerce (coerce)
import Data.Finite (Finite, natToFinite, modulo)
import Data.Foldable (find)
import Data.List (nub)
import Data.Tuple.Extra (dupe)
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

import Internal.Types(GLFWToken(UnsafeMkGLFWToken))

import Utils
import Shaders
import Types
import Swapchain

mkConfig :: Options -> Config
mkConfig (MkOptions w h f m _ l) = MkConfig (MkWindowSize w h) f m l

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
main = evalContT do
  opts <- lift $ execParser (Opt.info (options <**> helper) fullDesc)
  logOptions <- logOptionsHandle stderr (optVerbose opts)
  logFunc <- ContT $ withLogFunc logOptions
  runRIO (MkApp {logFunc, config = mkConfig opts}) $ catch runApp \e -> do
    logError $ display @SomeException e
    exitFailure

withGLFW :: MonadResource m => m (ReleaseKey, GLFWToken)
withGLFW = allocate
  do liftIO $ ifM GLFW.init (pure UnsafeMkGLFWToken)
                            (throwIO GLFWInitError)
  do const $ liftIO GLFW.terminate

withWindow :: (MonadReader env m, HasApp env, MonadResource m)
           => GLFWToken -> m (ReleaseKey, GLFW.Window)
withWindow !_ = do
  fullscreen <- view fullscreenL
  monitorIndex <- views monitorIndexL fromIntegral
  monitor <- preview (ix monitorIndex) . concat <$> liftIO GLFW.getMonitors
  when (fullscreen && isNothing monitor) $
    logWarn "Couldn't find desired monitor. Using windowed mode."
  (xPos, yPos, width, height) <- if
    | fullscreen, Just mon <- monitor -> liftIO $ GLFW.getMonitorWorkarea mon
    | otherwise -> join bitraverse (views ?? fromIntegral) (0, 0, windowWidthL, windowHeightL)

  allocate do traverse_ @[] GLFW.windowHint [ GLFW.WindowHint'ClientAPI GLFW.ClientAPI'NoAPI
                                            , GLFW.WindowHint'Resizable (not fullscreen)
                                            , GLFW.WindowHint'Decorated (not fullscreen)
                                            , GLFW.WindowHint'Floating fullscreen
                                            ]
              progName <- getProgName
              maybeM (throwIO GLFWWindowError)
                     (\window -> GLFW.setWindowPos window xPos yPos $> window)
                     (GLFW.createWindow width height progName Nothing Nothing)
           do GLFW.destroyWindow

withWindowSurface :: MonadResource m => Instance -> GLFW.Window -> m (ReleaseKey, SurfaceKHR)
withWindowSurface inst window = allocate
  do liftIO do surfacePtr <- malloc @SurfaceKHR
               result <- GLFW.createWindowSurface (instanceHandle inst) window nullPtr surfacePtr
               peek =<< catchVk (coerce @Int32 result, surfacePtr)
  do destroySurfaceKHR inst ?? Nothing
  where
    catchVk :: MonadIO m => (Result, a) -> m a
    catchVk = \case
      (SUCCESS, x) -> pure x
      (err    , _) -> throwIO $ VkGenericError err

drawFrame :: HasBoxticle env => Finite MaxFramesInFlight -> RIO env ()
drawFrame currentFrame = do
  device <- view deviceL
  swapchain <- viewRes swapchainL
  imageAvailable <- view imageAvailableL
  renderFinished <- view renderFinishedL
  inFlight <- view inFlightL
  graphicsQueue <- view graphicsQueueL
  presentQueue <- view presentQueueL

  let ixSync :: Sized.Vector MaxFramesInFlight a -> a
      ixSync = view $ Sized.ix currentFrame

  _result <- waitForFences device [ixSync inFlight] True maxBound

  (_, imageIndex) <-
    acquireNextImageKHR device swapchain maxBound (ixSync imageAvailable) NULL_HANDLE

  MkImageRelated{commandBuffer, imageInFlight} <- fromMaybeM
    do throwIO VkCommandBufferIndexOutOfRange
    do preview $ imageRelatedsL.ix (fromIntegral imageIndex)

  imageInFlightFence <- readIORef imageInFlight

  -- Check if a previous frame is using this image (i.e. there is its fence to wait on)
  whenJust imageInFlightFence \fence -> void $ waitForFences device [fence] True maxBound

  -- Mark the image as now being in use by this frame
  writeIORef imageInFlight (Just $ ixSync inFlight)

  let submitInfo = pure . SomeStruct $
        SubmitInfo{ next = ()
                  , waitSemaphores = [ixSync imageAvailable]
                  , waitDstStageMask = [PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT]
                  , commandBuffers = [commandBufferHandle commandBuffer]
                  , signalSemaphores = [ixSync renderFinished]
                  }

  resetFences device [ixSync inFlight]
  queueSubmit graphicsQueue submitInfo (ixSync inFlight)

  let presentInfo = PresentInfoKHR{ next = ()
                                  , waitSemaphores = [ixSync renderFinished]
                                  , swapchains = [swapchain]
                                  , imageIndices = [imageIndex]
                                  , results = zero
                                  }
  void $ queuePresentKHR presentQueue presentInfo

mainLoop :: HasBoxticle env => RIO env ()
mainLoop = do
  logDebug "Starting main loop."
  fix ?? natToFinite (Proxy @0) $ \loop currentFrame -> do
    drawFrame currentFrame
    liftIO GLFW.waitEvents
    let nextFrame = modulo @MaxFramesInFlight $ fromIntegral currentFrame + 1
    unlessM ?? loop nextFrame $ liftIO . GLFW.windowShouldClose =<< view windowL

  -- Allow queues/buffers to finish their job
  deviceWaitIdleSafe =<< view deviceL

validationLayers :: (MonadIO m, MonadReader env m, HasEnableValidationLayers env)
                 => m (Vector ByteString)
validationLayers = fmap V.fromList $ view enableValidationLayersL >>= bool (pure []) do
   (_, properties) <- enumerateInstanceLayerProperties
   case NE.nonEmpty $ filter (not . (properties `hasLayer`)) layers of
     Nothing          -> pure layers
     Just unsupported -> throwIO $ VkValidationLayersNotSupported unsupported
  where
    hasLayer props = V.elem ?? V.map layerName props
    layers = ["VK_LAYER_KHRONOS_validation"]

pickPhysicalDevice :: (MonadIO m, MonadReader env m, HasLogFunc env)
                   => Instance -> SurfaceKHR
                   -> m (PhysicalDevice, SwapchainSupport, QueueFamilyIndices)
pickPhysicalDevice inst surface = do
  enumeratePhysicalDevices inst >>= (snd >>> do
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
            do (fst . NE.maximumOn1 snd <$>) . mapM (traverseToSnd $ score . view _1))
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
      exts <- fmap extensionName . snd <$> enumerateDeviceExtensionProperties physDev Nothing
      pure $ V.all (`elem` exts) deviceExtensions

    querySwapchainSupport :: MonadIO m => PhysicalDevice -> m (Maybe SwapchainSupport)
    querySwapchainSupport physDev = runMaybeT do
      let formats      = getPhysicalDeviceSurfaceFormatsKHR physDev surface
          presentModes = getPhysicalDeviceSurfacePresentModesKHR physDev surface
      swapchainCapabilities      <- getPhysicalDeviceSurfaceCapabilitiesKHR physDev surface
      Just swapchainFormats      <- NE.nonEmpty . toList . snd <$> formats
      Just swapchainPresentModes <- NE.nonEmpty . toList . snd <$> presentModes
      pure $ MkSwapchainSupport{..}

deviceExtensions :: Vector ByteString
deviceExtensions =
  [ KHR_SWAPCHAIN_EXTENSION_NAME
  ]

withImageFences :: MonadResource m => Vector image -> m (Vector (image, IORef (Maybe Fence)))
withImageFences = traverse \image -> (image,) <$> newIORef Nothing

withImageViews :: MonadResource m
               => Device -> Format -> Vector (IORef Image, fence)
               -> m (Vector (IORef Image, fence, MResource ImageView))
withImageViews device format =
  traverse \(imageRef, fence) -> do
    image <- readIORef imageRef
    imageView <- mkMResource =<< withImageView device ivInfo{image} Nothing allocate
    pure (imageRef, fence, imageView)
  where
    ivInfo = zero{ viewType = IMAGE_VIEW_TYPE_2D
                 , format
                 , subresourceRange
                 }
    subresourceRange = zero{ aspectMask = IMAGE_ASPECT_COLOR_BIT
                           , levelCount = 1
                           , layerCount = 1
                           }

withFramebuffers :: MonadResource m
                 => Device -> Extent2D -> RenderPass -> Vector (image, fence, MResource ImageView)
                 -> m (Vector (image, fence, MResource ImageView, MResource Framebuffer))
withFramebuffers device Extent2D{width, height} renderPass images = do
  let fbInfo = zero{ renderPass
                   , width
                   , height
                   , layers = 1
                   }

  for images \(image, fence, imageViewRes) -> (image, fence, imageViewRes,) <$> do
    imageView <- imageViewRes^->resourceL
    mkMResource =<< withFramebuffer device fbInfo{attachments = [imageView]} Nothing allocate

setupCommands :: HasBoxticle env => RIO env ()
setupCommands = do
  view imageRelatedsL >>= traverse_ \MkImageRelated{commandBuffer, framebuffer = framebufferRes} ->
    useCommandBuffer commandBuffer zero do
      renderPass <- viewRes renderPassL
      extent <- viewRef swapchainExtentL
      graphicsPipeline <- viewRes graphicsPipelineL
      framebuffer <- framebufferRes^->resourceL
      let renderPassInfo = zero{ renderPass
                               , framebuffer
                               , renderArea = zero{extent}
                               , clearValues = [Color $ Float32 0 0 0 1]
                               }
      cmdUseRenderPass commandBuffer renderPassInfo SUBPASS_CONTENTS_INLINE do
        cmdBindPipeline commandBuffer PIPELINE_BIND_POINT_GRAPHICS graphicsPipeline
        cmdDraw commandBuffer 3 1 0 0
  logDebug "Set up commands."

withGraphicsResources :: (MonadResource m, MonadReader env m, HasApp env)
                      => m GraphicsResources
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

  (_, inst) <- withInstance instanceCreateInfo Nothing allocate
  (_, window) <- withWindow glfwToken
  logDebug "Created window."

  (_, surface) <- withWindowSurface inst window
  logDebug "Created surface."

  (physDev, swapchainSupport, queueFamilyIndices@(MkQueueFamilyIndices{..})) <-
    pickPhysicalDevice inst surface
  logDebug "Picked device."

  let queueCreateInfos = V.fromList (nub [graphicsQueueFamily, presentQueueFamily]) <&>
        \index -> SomeStruct zero{queueFamilyIndex = index, queuePriorities = [1]}
      deviceCreateInfo = zero{ queueCreateInfos
                             , enabledLayerNames
                             , enabledExtensionNames = deviceExtensions
                             }
  (_, device) <- withDevice physDev deviceCreateInfo Nothing allocate
  logDebug "Created logical device."

  queues <- uncurry MkQueues <$>
    join bitraverse (getDeviceQueue device ?? 0) (graphicsQueueFamily, presentQueueFamily)
  logDebug "Obtained device queues."

  pure $ MkGraphicsResources{..}

runApp :: HasApp env => RIO env ()
runApp = runResourceT do
  liftIO compileAllShaders
  logDebug "Compiled shaders."

  logDebug "Started boxticle."

  graphicsResources <- withGraphicsResources

  app <- view appL
  let graphicsApp = MkGraphicsApp{..}

  swapchainRelated <- runReaderT withSwapchain graphicsApp
  swapchain <- runReaderT (viewRes swapchainL) swapchainRelated

  let device = graphicsResources^.deviceL
      commandPoolInfo = zero{ queueFamilyIndex = graphicsResources^.graphicsQueueFamilyL
                            } :: CommandPoolCreateInfo
  (_, commandPool) <- withCommandPool device commandPoolInfo Nothing allocate
  logDebug "Created command pool."

  images <- traverse newIORef . snd =<< getSwapchainImagesKHR device swapchain
  imgWithFences <- withImageFences images

  let SurfaceFormatKHR{format} = swapchainRelated^.swapchainFormatL
  imgsWithViews <- withImageViews device format imgWithFences

  swapchainExtent <- readIORef (swapchainRelated^.swapchainExtentL)
  renderPass <- runReaderT (viewRes renderPassL) swapchainRelated
  ivsWithFbs <- withFramebuffers device swapchainExtent renderPass imgsWithViews

  let commandBuffersInfo = zero{ commandPool
                               , level = COMMAND_BUFFER_LEVEL_PRIMARY
                               , commandBufferCount = fromIntegral $ length ivsWithFbs
                               }
  (_, commandBuffers) <- withCommandBuffers device commandBuffersInfo allocate
  imageRelateds <- V.zipWithM
        (\(image, imageInFlight, imageView, framebuffer) commandBuffer -> pure MkImageRelated{..})
        ivsWithFbs commandBuffers
  logDebug "Created buffers."

  (imageAvailable, renderFinished) <- bisequence . dupe . Sized.replicateM $
    snd <$> withSemaphore device zero Nothing allocate
  inFlight <- Sized.replicateM $
    snd <$> withFence device zero{flags = FENCE_CREATE_SIGNALED_BIT} Nothing allocate
  let syncs = MkSyncs{..}
  logDebug "Created syncs."

  lift . mapRIO (const MkBoxticle{..}) $ setupCommands *> mainLoop

  logInfo "Goodbye!"
