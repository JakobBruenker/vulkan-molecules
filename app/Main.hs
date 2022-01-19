{-# LANGUAGE OverloadedLists #-}

module Main where

import RIO
import RIO.ByteString qualified as B
import RIO.NonEmpty qualified as NE
import RIO.Vector qualified as V
import Data.List.NonEmpty.Extra qualified as NE

import Control.Lens (views, (??), _1, both, ix)
import Control.Monad.Extra (fromMaybeM, ifM, whenJust)
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

import Utils (traverseToSnd, plural, clamp, bracketCont)
import Foreign (malloc, nullPtr, Storable (peek))
import Data.Coerce (coerce)
import Data.Foldable (find)
import Control.Applicative (ZipList(..))
import Data.List (nub)
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad.Trans.Cont (evalContT, ContT(..))

import Shaders (compileAllShaders)
import Types
import Pipeline (withGraphicsPipeline)
import Data.Tuple.Extra (dupe)
import Data.Finite (Finite, natToFinite, modulo)

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
               peek =<< catchVk (coerce @Int32 result, surfacePtr)
  do destroySurfaceKHR inst ?? Nothing
  where
    catchVk :: MonadIO m => (Result, a) -> m a
    catchVk = \case
      (SUCCESS, x) -> pure x
      (err    , _) -> throwIO $ VkGenericError err

drawFrame :: HasVulkanApp env => Finite MaxFramesInFlight -> RIO env ()
drawFrame currentFrame = do
  device <- view deviceL
  swapchain <- view swapchainL
  imageAvailable <- view imageAvailableL
  renderFinished <- view renderFinishedL
  inFlight <- view inFlightL
  graphicsQueue <- view graphicsQueueL
  presentQueue <- view presentQueueL

  let ixSync :: Sized.Vector MaxFramesInFlight a -> a
      ixSync = view $ Sized.ix currentFrame

  _result <- waitForFences device [ixSync inFlight] True maxBound

  imageIndex <- snd <$>
    acquireNextImageKHR device swapchain maxBound (ixSync imageAvailable) NULL_HANDLE

  MkBufferCollection{commandBuffer, imageInFlight} <- fromMaybeM
    do throwIO VkCommandBufferIndexOutOfRange
    do preview $ buffersL.ix (fromIntegral imageIndex)

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

mainLoop :: HasVulkanApp env => RIO env ()
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
   properties <- snd <$> enumerateInstanceLayerProperties
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

    querySwapchainSupport :: MonadIO m => PhysicalDevice -> m (Maybe SwapchainSupportDetails)
    querySwapchainSupport physDev = runMaybeT do
      let formats      = getPhysicalDeviceSurfaceFormatsKHR physDev surface
          presentModes = getPhysicalDeviceSurfacePresentModesKHR physDev surface
      swapchainCapabilities      <- getPhysicalDeviceSurfaceCapabilitiesKHR physDev surface
      Just swapchainFormats      <- NE.nonEmpty . toList . snd <$> formats
      Just swapchainPresentModes <- NE.nonEmpty . toList . snd <$> presentModes
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
    presentMode = PRESENT_MODE_MAILBOX_KHR -- TODO select FIFO if MAILBOX is not available
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

withImageFences :: MonadUnliftIO m => Vector Image -> ContT r m (Vector (Image, IORef (Maybe Fence)))
withImageFences = traverse \image -> (image,) <$> newIORef Nothing

withImageViews :: MonadUnliftIO m
               => Device -> Format -> Vector (Image, fence)
               -> ContT r m (Vector (Image, fence, ImageView))
withImageViews device format =
  traverse \(image, fence) -> (image, fence,) <$> withImageView device ivInfo{image} Nothing bracketCont
  where
    ivInfo = zero{ viewType = IMAGE_VIEW_TYPE_2D
                 , format
                 , subresourceRange
                 }
    subresourceRange = zero{ aspectMask = IMAGE_ASPECT_COLOR_BIT
                           , levelCount = 1
                           , layerCount = 1
                           }

withFramebuffers :: MonadUnliftIO m
                 => GraphicsResources -> PipelineDetails -> Vector (Image, fence, ImageView)
                 -> ContT r m (Vector (Image, fence, ImageView, Framebuffer))
withFramebuffers graphicsResources MkPipelineDetails{renderPass} = traverse
  \(image, fence, imageView) -> (image, fence, imageView,) <$>
    withFramebuffer device fbInfo{attachments = [imageView]} Nothing bracketCont
  where
    device = graphicsResources^.deviceL
    Extent2D{width, height} = graphicsResources^.swapchainExtentL

    fbInfo = zero{ renderPass
                 , width
                 , height
                 , layers = 1
                 }

setupCommands :: HasVulkanApp env => RIO env ()
setupCommands = do
  view buffersL >>= traverse_ \MkBufferCollection{commandBuffer, framebuffer} ->
    useCommandBuffer commandBuffer zero do
      renderPass <- view renderPassL
      extent <- view swapchainExtentL
      graphicsPipeline <- view pipelineL
      let renderPassInfo = zero{ renderPass
                               , framebuffer
                               , renderArea = zero{extent}
                               , clearValues = [Color $ Float32 0 0 0 1]
                               }
      cmdUseRenderPass commandBuffer renderPassInfo SUBPASS_CONTENTS_INLINE do
        cmdBindPipeline commandBuffer PIPELINE_BIND_POINT_GRAPHICS graphicsPipeline
        cmdDraw commandBuffer 3 1 0 0
  logDebug "Set up commands."

withGraphicsResources :: (MonadUnliftIO m, MonadReader env m, HasApp env)
                         => ContT r m GraphicsResources
withGraphicsResources = do
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

  let swapchainFormat  = SurfaceFormatKHR imageFormat imageColorSpace
      swapchainDetails = MkSwapchainDetails{..}

  pure $ MkGraphicsResources{..}

runApp :: HasApp env => RIO env ()
runApp = evalContT do
  liftIO compileAllShaders
  logDebug "Compiled shaders."

  logDebug "Started boxticle."

  graphicsResources <- withGraphicsResources

  pipelineDetails <- withGraphicsPipeline graphicsResources
  logDebug "Created pipeline."

  let device = graphicsResources^.deviceL
      swapchain = graphicsResources^.swapchainL
      commandPoolInfo = zero{ queueFamilyIndex = graphicsResources^.graphicsQueueFamilyL
                            } :: CommandPoolCreateInfo
  commandPool <- withCommandPool device commandPoolInfo Nothing bracketCont
  logDebug "Created command pool."

  images <- snd <$> getSwapchainImagesKHR device swapchain
  imgWithFences <- withImageFences images

  let SurfaceFormatKHR{format} = graphicsResources^.swapchainFormatL
  imgsWithViews <- withImageViews device format imgWithFences

  ivsWithFbs <- withFramebuffers graphicsResources pipelineDetails imgsWithViews

  let commandBuffersInfo = zero{ commandPool
                               , level = COMMAND_BUFFER_LEVEL_PRIMARY
                               , commandBufferCount = fromIntegral $ length ivsWithFbs
                               }
  commandBuffers <- withCommandBuffers device commandBuffersInfo bracketCont
  let buffers = V.zipWith (\(image, imageInFlight, imageView, framebuffer) commandBuffer -> MkBufferCollection{..})
                          ivsWithFbs commandBuffers
  logDebug "Created buffers."

  (imageAvailable, renderFinished) <- bisequence . dupe . Sized.replicateM $
    withSemaphore device zero Nothing bracketCont
  inFlight <- Sized.replicateM $
    withFence device zero{flags = FENCE_CREATE_SIGNALED_BIT} Nothing bracketCont
  let syncs = MkSyncs{..}
  logDebug "Created syncs."

  lift . mapRIO (\env -> MkVulkanApp {app = env^.appL, ..}) $ setupCommands *> mainLoop
