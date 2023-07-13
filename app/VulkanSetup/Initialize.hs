{-# LANGUAGE OverloadedLists #-}

module VulkanSetup.Initialize where

import RIO hiding (logDebug, logInfo, logWarn, logError)
import RIO.ByteString qualified as B
import RIO.NonEmpty qualified as NE
import RIO.Vector qualified as V
import RIO.Vector.Storable.Unsafe qualified as VS

import Control.Lens ((??), ix)
import Control.Monad.Extra (fromMaybeM, ifM, maybeM)
import Data.Bits (Bits((.&.)), testBit, (.|.), xor)
import Data.Semigroup (Arg(..), Max (..), Semigroup (sconcat))
import System.Environment (getProgName)
import Control.Applicative (ZipList(..))
import Control.Monad.Trans.Maybe (runMaybeT, MaybeT)
import Control.Monad.Trans.Resource (allocate, ResIO, MonadResource, ReleaseKey, release)
import Data.Coerce (coerce)
import Data.Foldable (find)
import Data.List (nub)
import Foreign (malloc, nullPtr, Storable (peek, sizeOf), copyBytes, castPtr)
import Foreign.Storable.Tuple ()
import Control.Lens.Combinators (ifind)
import Data.Vector.Storable.Sized qualified as Sized
import Data.Tuple.Extra (dupe, both)
import GHC.TypeNats (KnownNat)

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


import Utils
import VulkanSetup.Types.Unsafe(GLFWToken(UnsafeMkGLFWToken))
import VulkanSetup.GraphicsMutables
import VulkanSetup.Utils
import VulkanSetup.Types
import VulkanSetup.ComputeMutables

initGLFW :: HasLogger => ResIO (Dict HasGLFW)
initGLFW = do
  (_, glfwToken) <- allocate
    do liftIO $ ifM GLFW.init do pure UnsafeMkGLFWToken
                              do throwIO GLFWInitError
    do const $ liftIO GLFW.terminate
  let ?glfw = glfwToken

  logDebug "InitializedGLFW."
  pure Dict

initInstance :: (HasLogger, HasValidationLayers) => ResIO (Dict HasInstance)
initInstance = do
  unlessM (liftIO GLFW.vulkanSupported) $ throwIO GLFWVulkanNotSupported
  logDebug "Verified GLFW Vulkan support."

  enabledExtensionNames <- V.fromList <$>
    do liftIO GLFW.getRequiredInstanceExtensions >>= liftIO . mapM B.packCString
  logDebug $ "Required extensions for GLFW: " <> displayShow enabledExtensionNames

  let applicationInfo = Just (zero{apiVersion = MAKE_API_VERSION 1 2 0} :: ApplicationInfo)
      instanceCreateInfo = zero{ applicationInfo
                               , enabledExtensionNames
                               , enabledLayerNames = ?validationLayers
                               }

  (_, inst) <- withInstance instanceCreateInfo Nothing allocate
  let ?instance = inst

  logDebug "Created instance."
  pure Dict

initWindow :: (HasLogger, HasConfig, HasGLFW) => ResIO (Dict (HasWindow, HasFramebufferResized))
initWindow = do
  !_ <- pure ?glfw -- making sure GLFW really is initialized

  framebufferResized <- newIORef False

  monitor <- preview (ix $ fromIntegral ?monitorIndex) . concat <$> liftIO GLFW.getMonitors
  when (?fullscreen && isNothing monitor) $
    logWarn "Couldn't find desired monitor. Using windowed mode."
  let actuallyFullscreen = ?fullscreen && isJust monitor
  logDebug $ "Fullscreen: " <> displayShow actuallyFullscreen
  ((xPos, yPos), (width, height)) <- fromMaybe
    ((0, 0), (fromIntegral ?windowWidth, fromIntegral ?windowHeight)) <$> if
    | actuallyFullscreen, Just mon <- monitor -> do
      videoMode <- liftIO $ GLFW.getVideoMode mon
      if | Just vm <- videoMode -> do
           pos <- liftIO $ GLFW.getMonitorPos mon
           pure $ Just (pos, (vm.videoModeWidth, vm.videoModeHeight))
         | otherwise -> do
           logWarn "Couldn't get video mode. Using default window size."
           pure Nothing
    | otherwise -> pure Nothing
  logDebug $ "Window position: " <> displayShow (xPos, yPos)
  logDebug $ "Window size: " <> displayShow (width, height)

  (_, window) <- allocate
    do traverse_ @[] GLFW.windowHint [ GLFW.WindowHint'ClientAPI GLFW.ClientAPI'NoAPI
                                     , GLFW.WindowHint'Resizable (not actuallyFullscreen)
                                     , GLFW.WindowHint'Decorated (not actuallyFullscreen)
                                     , GLFW.WindowHint'Floating actuallyFullscreen
                                     ]
       progName <- getProgName
       maybeM do throwIO GLFWWindowError
              do \window -> do GLFW.setWindowPos window xPos yPos
                               GLFW.setFramebufferSizeCallback window $ Just \_ _ _ ->
                                 writeIORef framebufferResized True
                               pure window
              do GLFW.createWindow width height progName Nothing Nothing
    do GLFW.destroyWindow
  let ?window = window
      ?framebufferResized = framebufferResized

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
      do throwIO VkNoPhysicalDevices
      \(toList -> physDevs) -> do
        let lds = length physDevs
        logDebug . (("Found " <> display lds <> plural " physical device" lds <> ": ") <>) .
          displayBytesUtf8 . B.intercalate ", " 
          =<< traverse (fmap deviceName <$> getPhysicalDeviceProperties) physDevs

        fmap ((\(Arg _ x) -> x) . getMax . sconcat) .
          fromMaybeM (throwIO VkNoSuitableDevices) . fmap NE.nonEmpty $
            mapMaybeM ?? physDevs $ \physDev -> runMaybeT do
              let ?physicalDevice = physDev
              guard =<< checkDeviceExtensionSupport
              Dict <- querySwapchainSupport
              Dict <- findQueueFamilies
              Dict <- getSampleCount
              Max . flip Arg Dict <$> score

  case dict of
    Dict -> logDebug . ("Picked device " <>) . (<> ".") . displayBytesUtf8 . deviceName =<<
      getPhysicalDeviceProperties ?physicalDevice
  pure dict
  where
    score :: (MonadIO m, HasPhysicalDevice) => m Integer
    score = (bool 0 1000 . (PHYSICAL_DEVICE_TYPE_DISCRETE_GPU ==) . deviceType <$>) $
      getPhysicalDeviceProperties ?physicalDevice

    findQueueFamilies :: (MonadResource m, HasLogger, HasPhysicalDevice)
                      => MaybeT m (Dict HasQueueFamilyIndices)
    findQueueFamilies = do
      dName <- devName
      props <- getPhysicalDeviceQueueFamilyProperties ?physicalDevice

      let findByFlag flag name = do
            Just queue <- pure $ fromIntegral <$>
              V.findIndex (\p -> queueFlags p .&. flag > zero) props
            logResult name queue dName
            pure queue

      graphics <- findByFlag QUEUE_GRAPHICS_BIT "graphics"

      let indices = ZipList (toList props) *> [0..]
          surfaceSupport = getPhysicalDeviceSurfaceSupportKHR ?physicalDevice ?? ?surface
      Just present <- fmap fst . find snd <$> (traverse . traverseToSnd) surfaceSupport indices
      logResult "present" present dName

      compute <- findByFlag QUEUE_COMPUTE_BIT "compute"

      let ?graphicsQueueFamily = graphics
          ?presentQueueFamily  = present
          ?computeQueueFamily  = compute
      pure Dict
      where
        logResult name queueIndex dName = logDebug $
          "Found " <> name <> " queue family (index " <> display queueIndex <> ") on " <>
          dName <> "."

    checkDeviceExtensionSupport :: (MonadIO m, HasPhysicalDevice) => m Bool
    checkDeviceExtensionSupport = do
      exts <- fmap extensionName . snd <$> enumerateDeviceExtensionProperties ?physicalDevice Nothing
      let yes = V.all (`elem` exts) deviceExtensions
      dName <- devName
      logDebug $ dName <> " " <> bool "doesn't support" "supports" yes <>
                 " all extensions in " <> displayShow deviceExtensions <> "."
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

    getSampleCount :: (MonadIO m, HasPhysicalDevice) => m (Dict HasMsaaSamples)
    getSampleCount = do
      let desired = SAMPLE_COUNT_4_BIT
      devFeatures <- getPhysicalDeviceFeatures ?physicalDevice
      devLimits <- getPhysicalDeviceProperties ?physicalDevice <&> \p -> p.limits
      let ?msaaSamples = if   devFeatures.sampleRateShading
                           && devLimits.framebufferColorSampleCounts
                          .&. devLimits.framebufferDepthSampleCounts
                          .&. desired /= zero
                         then desired
                         else SAMPLE_COUNT_1_BIT
      dName <- devName
      logDebug $ "Using " <> displayShow ?msaaSamples <> " for MSAA on " <> dName <> "."
      pure Dict

    devName :: (MonadIO m, HasPhysicalDevice) => m Utf8Builder
    devName = displayBytesUtf8 . deviceName <$> getPhysicalDeviceProperties ?physicalDevice

initDevice :: (HasLogger, HasPhysicalDeviceRelated, HasValidationLayers)
           => ResIO (Dict HasDevice)
initDevice = do
  let queueCreateInfos =
        V.fromList (nub [?graphicsQueueFamily, ?presentQueueFamily, ?computeQueueFamily]) <&>
          \index -> SomeStruct zero{queueFamilyIndex = index, queuePriorities = [1, 0.5]}
      deviceCreateInfo = zero{ queueCreateInfos
                             , enabledLayerNames = ?validationLayers
                             , enabledExtensionNames = deviceExtensions
                             , enabledFeatures =
                                 Just zero{sampleRateShading = ?msaaSamples /= SAMPLE_COUNT_1_BIT}
                             }
  (_, device) <- withDevice ?physicalDevice deviceCreateInfo Nothing allocate
  let ?device = device

  logDebug "Created logical device."
  pure Dict

initQueues :: (HasLogger, HasDevice, HasQueueFamilyIndices) => ResIO (Dict HasQueues)
initQueues = do
  let getQueue = getDeviceQueue ?device
  graphicsQueue <- getQueue ?graphicsQueueFamily 0
  presentQueue  <- getQueue ?presentQueueFamily  0
  -- Is it better to use a compute queue that's different from the graphics
  -- queue? Not sure. Doing it for now though.
  computeQueue  <- getQueue ?computeQueueFamily  1
  let ?graphicsQueue = graphicsQueue
      ?presentQueue  = presentQueue
      ?computeQueue  = computeQueue

  logDebug "Obtained device queues."
  pure Dict

deviceExtensions :: Vector ByteString
deviceExtensions =
  [ KHR_SWAPCHAIN_EXTENSION_NAME
  , KHR_STORAGE_BUFFER_STORAGE_CLASS_EXTENSION_NAME
  , KHR_SHADER_NON_SEMANTIC_INFO_EXTENSION_NAME -- XXX JB Only used if debug flag is enabled
  ]

initCommandPools :: (HasLogger, HasDevice, HasGraphicsQueueFamily, HasComputeQueueFamily)
                 => ResIO (Dict (HasGraphicsCommandPool, HasComputeCommandPool))
initCommandPools = do
  let graphicsCommandPoolInfo = zero{ queueFamilyIndex = ?graphicsQueueFamily
                                    } :: CommandPoolCreateInfo
  (_, graphicsPool) <- withCommandPool ?device graphicsCommandPoolInfo Nothing allocate
  let ?graphicsCommandPool = graphicsPool

  let computeCommandPoolInfo = zero{ queueFamilyIndex = ?computeQueueFamily
                                   } :: CommandPoolCreateInfo
  (_, computePool) <- withCommandPool ?device computeCommandPoolInfo Nothing allocate
  let ?computeCommandPool = computePool

  logDebug "Created command pools."
  pure Dict

-- TODO: combine buffers and allocations into single buffers and use offsets to
-- bind, where it makes sense
constructBuffer :: (HasPhysicalDevice, HasDevice)
                => BufferCreateInfo '[] -> MemoryPropertyFlags
                -> ResIO ((ReleaseKey, Buffer), (ReleaseKey, DeviceMemory))
constructBuffer info properties = do
  (bufferKey, buffer) <- withBuffer ?device info Nothing allocate

  memReqs <- getBufferMemoryRequirements ?device buffer
  memProps <- getPhysicalDeviceMemoryProperties ?physicalDevice

  let memoryType = ifind ?? memProps.memoryTypes $ \i MemoryType{propertyFlags} ->
        testBit memReqs.memoryTypeBits i &&
        -- check whether all properties are set
        ((propertyFlags `xor` properties) .&. properties == zero)

  memoryTypeIndex <- maybe (throwIO VkNoSuitableMemoryType) (pure . fromIntegral . fst) memoryType

  let allocInfo = zero{allocationSize = memReqs.size, memoryTypeIndex}
  (memoryKey, memory) <- withMemory ?device allocInfo Nothing allocate

  bindBufferMemory ?device buffer memory 0

  pure ((bufferKey, buffer), (memoryKey, memory))

copyBuffer :: (MonadUnliftIO m, HasDevice)
           => CommandPool -> Queue -> "src" ::: Buffer -> "dst" ::: Buffer -> DeviceSize -> m ()
copyBuffer commandPool queue src dst size = do
  let allocInfo = zero{ level = COMMAND_BUFFER_LEVEL_PRIMARY
                      , commandPool
                      , commandBufferCount = 1
                      }
  let beginInfo = zero{flags = COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT} :: CommandBufferBeginInfo '[]
  withCommandBuffers ?device allocInfo bracket \case
    [cmdBuffer] -> do
      useCommandBuffer cmdBuffer beginInfo do
        cmdCopyBuffer cmdBuffer src dst [zero{size} :: BufferCopy]
      let submitInfo = SomeStruct zero{commandBuffers = [commandBufferHandle cmdBuffer]}
      queueSubmit queue [submitInfo] NULL_HANDLE
      queueWaitIdle queue
      pure ()
    bufs -> throwIO $ VkWrongNumberOfCommandBuffers 1 (fromIntegral $ length bufs)

initVertexBuffer :: (HasLogger, HasPhysicalDevice, HasDevice, HasGraphicsQueue, HasGraphicsCommandPool,
                     HasVertexBufferInfo, HasVertexData)
                 => ResIO (Dict HasVertexBuffer)
initVertexBuffer = do
  let stagingBufInfo = ?vertexBufferInfo{ usage = BUFFER_USAGE_TRANSFER_SRC_BIT
                                        , sharingMode = SHARING_MODE_EXCLUSIVE
                                        } :: BufferCreateInfo '[]
      stagingBufProps = MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. MEMORY_PROPERTY_HOST_COHERENT_BIT

  ((sBufKey, stagingBuffer), (sMemKey, stagingMemory)) <-
    constructBuffer stagingBufInfo stagingBufProps

  liftIO $ withMappedMemory ?device stagingMemory 0 ?vertexBufferInfo.size zero bracket \target -> do
    MkVertexData vertexData <- pure ?vertexData
    VS.unsafeWith (Sized.SomeSized vertexData) \(castPtr -> source) ->
      copyBytes target source $ fromIntegral ?vertexBufferInfo.size

  let vertBufProps = MEMORY_PROPERTY_DEVICE_LOCAL_BIT
  ((_, vertexBuffer), _) <- constructBuffer ?vertexBufferInfo vertBufProps
  let ?vertexBuffer = vertexBuffer

  copyBuffer ?graphicsCommandPool ?graphicsQueue stagingBuffer vertexBuffer ?vertexBufferInfo.size

  -- We don't need the staging buffer anymore
  traverse_ @[] release [sBufKey, sMemKey]

  logDebug "Created vertex buffer."
  pure Dict

-- TODO factor out common code with initVertexBuffer
initComputeStorageBuffers :: (HasLogger, HasPhysicalDevice, HasDevice, HasComputeCommandPool,
                              HasComputeQueue, HasComputeStorageData)
                          => ResIO (Dict HasComputeStorageBuffers)
initComputeStorageBuffers = do
  storageBuffers <- for ?computeStorageData \(MkStorageData storageData) -> do
    let size = fromIntegral $ sizeOf storageData
        stagingBufInfo = zero{ size
                             , usage = BUFFER_USAGE_TRANSFER_SRC_BIT
                             , sharingMode = SHARING_MODE_EXCLUSIVE
                             }
        stagingBufProps = MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. MEMORY_PROPERTY_HOST_COHERENT_BIT
    ((sBufKey, stagingBuffer), (sMemKey, stagingMemory)) <-
      constructBuffer stagingBufInfo stagingBufProps

    liftIO $ withMappedMemory ?device stagingMemory 0 size zero bracket \target -> do
      VS.unsafeWith (Sized.SomeSized storageData) \(castPtr -> source) ->
        copyBytes target source $ sizeOf storageData

    let storageBufInfo = stagingBufInfo{ usage = BUFFER_USAGE_TRANSFER_DST_BIT
                                             .|. BUFFER_USAGE_STORAGE_BUFFER_BIT
                                       } :: BufferCreateInfo '[]
        storageBufProps = MEMORY_PROPERTY_DEVICE_LOCAL_BIT
    ((_, storageBuffer), _) <- constructBuffer storageBufInfo storageBufProps

    copyBuffer ?computeCommandPool ?computeQueue stagingBuffer storageBuffer size

    -- We don't need the staging buffer anymore
    traverse_ @[] release [sBufKey, sMemKey]

    pure storageBuffer
  let ?computeStorageBuffers = storageBuffers

  logDebug "Created compute storage buffers."
  pure Dict

initGraphicsUniformBuffers :: (HasLogger, HasSurface, HasPhysicalDevice, HasDevice,
                               HasGraphicsUniformBufferSize, HasDesiredSwapchainImageNum)
                           => ResIO (Dict HasGraphicsUniformBuffers)
initGraphicsUniformBuffers = do
  surfCaps <- getPhysicalDeviceSurfaceCapabilitiesKHR ?physicalDevice ?surface

  -- We need at least as many buffers as we have images
  let numBuffers = max (fromIntegral surfCaps.minImageCount) (fromIntegral ?desiredSwapchainImageNum)
      bufInfo = zero{ size = ?graphicsUniformBufferSize
                    , usage = BUFFER_USAGE_UNIFORM_BUFFER_BIT
                    , sharingMode = SHARING_MODE_EXCLUSIVE
                    } :: BufferCreateInfo '[]
      properties = MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. MEMORY_PROPERTY_HOST_COHERENT_BIT
  buffers <- V.replicateM numBuffers do
    ((_, buffer), (_, memory)) <- constructBuffer bufInfo properties
    pure (buffer, memory)
  let ?graphicsUniformBuffers = buffers

  logDebug "Created uniform buffers."

  pure Dict

initComputeUniformBuffer :: (HasLogger, HasPhysicalDevice, HasDevice,
                             HasComputeUniformBufferSize)
                         => ResIO (Dict HasComputeUniformBuffer)
initComputeUniformBuffer = do
  let bufInfo = zero{ size = ?computeUniformBufferSize
                    , usage = BUFFER_USAGE_UNIFORM_BUFFER_BIT
                    , sharingMode = SHARING_MODE_EXCLUSIVE
                    } :: BufferCreateInfo '[]
      properties = MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. MEMORY_PROPERTY_HOST_COHERENT_BIT
  ((_, buffer), (_, memory)) <- constructBuffer bufInfo properties
  let ?computeUniformBuffer = (buffer, memory)

  logDebug "Created uniform buffers."

  pure Dict

initGraphicsDescriptorSetLayout :: (HasDevice, HasGraphicsDescriptorSetLayoutInfo)
                                => ResIO (Dict HasGraphicsDescriptorSetLayout)
initGraphicsDescriptorSetLayout = do
  (_, layout) <- withDescriptorSetLayout ?device ?graphicsDescriptorSetLayoutInfo Nothing allocate
  let ?graphicsDescriptorSetLayout = layout
  pure Dict

initComputeDescriptorSetLayout :: (HasDevice, HasComputeDescriptorSetLayoutInfo)
                               => ResIO (Dict HasComputeDescriptorSetLayouts)
initComputeDescriptorSetLayout = do
  layouts <- forM ?computeDescriptorSetLayoutInfo \layoutInfo ->
    snd <$> withDescriptorSetLayout ?device layoutInfo Nothing allocate
  let ?computeDescriptorSetLayouts = layouts
  pure Dict

initVertexStorageBuffer :: HasVertexBuffer => ResIO (Dict HasVertexStorageBuffer)
initVertexStorageBuffer = do
  let ?vertexStorageBuffer = ?vertexBuffer
  pure Dict

initComputeFences :: HasDevice => ResIO (Dict HasComputeFences)
initComputeFences = do
  fences <- both snd <$> bisequence
    (dupe $ withFence ?device zero{flags = FENCE_CREATE_SIGNALED_BIT} Nothing allocate)

  let ?computeFences = fences
  pure Dict

initSyncs :: (HasLogger, HasDevice) => ResIO (Dict HasSyncs)
initSyncs = do
  (imageAvailable, renderFinished) <- bisequence . dupe . Sized.replicateM $
    snd <$> withSemaphore ?device zero Nothing allocate
  inFlight <- Sized.replicateM $
    snd <$> withFence ?device zero{flags = FENCE_CREATE_SIGNALED_BIT} Nothing allocate
  let ?imageAvailable = imageAvailable
      ?renderFinished = renderFinished
      ?inFlight       = inFlight

  logDebug "Created syncs."
  pure Dict

initializeVulkan :: (HasLogger, HasConfig, HasShaderPaths, HasVulkanConfig,
                     KnownNat ComputeShaderCount, KnownNat ComputeStorageBufferCount)
                 => (HasVulkanResources => ResIO ()) -> (HasVulkanResources => ResIO ())
                 -> ResIO (Dict HasVulkanResources)
initializeVulkan setupGraphicsCommands setupComputeCommands = do
  Dict <- initGLFW
  Dict <- initValidationLayers
  Dict <- initWindow
  Dict <- initInstance
  Dict <- initSurface
  Dict <- initPhysicalDevice
  Dict <- initDevice
  Dict <- initQueues
  Dict <- initCommandPools
  Dict <- initVertexBuffer
  Dict <- initGraphicsUniformBuffers
  Dict <- initGraphicsDescriptorSetLayout
  Dict <- initGraphicsMutables
  Dict <- initComputeUniformBuffer
  Dict <- initComputeDescriptorSetLayout
  Dict <- initVertexStorageBuffer
  Dict <- initComputeStorageBuffers
  Dict <- initComputeFences
  Dict <- initComputeMutables
  Dict <- initSyncs

  setupGraphicsCommands *> setupComputeCommands $> Dict
