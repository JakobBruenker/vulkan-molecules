{-# LANGUAGE OverloadedLists #-}

module VulkanSetup.Initialize where

import RIO hiding (logDebug, logInfo, logWarn, logError)
import RIO.ByteString qualified as B
import RIO.NonEmpty qualified as NE
import RIO.Vector qualified as V
import RIO.Vector.Storable.Unsafe qualified as VS

import Control.Lens ((??), ix)
import Control.Monad.Extra (fromMaybeM, maybeM)
import Data.Bits (Bits((.&.)), testBit, (.|.), xor)
import Data.Semigroup (Arg(..), Max (..), Semigroup (sconcat))
import System.Environment (getProgName)
import Control.Applicative (ZipList(..))
import Control.Monad.Trans.Maybe (runMaybeT, MaybeT(..))
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
import VulkanSetup.Error
import VulkanConfig.Pipeline (graphicsUniformBufferSize, computeUniformBufferSize)

initGLFW :: HasLogger => ResIO (Dict HasGLFW)
initGLFW = do
  let ?glfw = snd !do
        allocate
          do if | !GLFW.init -> pure UnsafeMkGLFWToken
                | otherwise  -> throw GLFWInitError
          do const $ liftIO GLFW.terminate

  logDebug "InitializedGLFW."
  pure Dict

initInstance :: (HasLogger, HasValidationLayers) => ResIO (Dict HasInstance)
initInstance = do
  unlessM (liftIO GLFW.vulkanSupported) $ throw GLFWVulkanNotSupported
  logDebug "Verified GLFW Vulkan support."

  enabledExtensionNames <- V.fromList <$>
    do liftIO GLFW.getRequiredInstanceExtensions >>= liftIO . mapM B.packCString
  logDebug $ "Required instance extensions for GLFW: " <> displayShow enabledExtensionNames

  let applicationInfo = Just (zero{apiVersion = MAKE_API_VERSION 1 2 0} :: ApplicationInfo)
      instanceCreateInfo = zero{ applicationInfo
                               , enabledExtensionNames
                               , enabledLayerNames = ?validationLayers
                               }

  let ?instance = snd !do withInstance instanceCreateInfo Nothing allocate

  logDebug "Created instance."
  pure Dict

initWindow :: (HasLogger, HasConfig, HasGLFW) => ResIO (Dict (HasWindow, HasFramebufferResized))
initWindow = do
  !_ <- pure ?glfw -- making sure GLFW really is initialized

  let ?framebufferResized = !do newIORef False

  monitor <- preview (ix $ fromIntegral ?monitorIndex) . concat <$> liftIO GLFW.getMonitors
  when (?fullscreen && isNothing monitor) $
    logWarn "Couldn't find desired monitor. Using windowed mode."
  let actuallyFullscreen = ?fullscreen && isJust monitor
  logDebug $ "Fullscreen: " <> displayShow actuallyFullscreen
  ((xPos, yPos), (width, height)) <- fromMaybe
    ((0, 0), (fromIntegral ?windowWidth, fromIntegral ?windowHeight)) <$> if
    | actuallyFullscreen, Just mon <- monitor ->
      liftIO $ GLFW.getVideoMode mon >>= \case
        Just vm -> do
          let windowPos = !do liftIO $ GLFW.getMonitorPos mon
              windowSize = (vm.videoModeWidth, vm.videoModeHeight)
          pure $ Just (windowPos, windowSize)
        Nothing -> do
          logWarn "Couldn't get video mode. Using default window size."
          pure Nothing
    | otherwise -> pure Nothing
  logDebug $ "Window position: " <> displayShow (xPos, yPos)
  logDebug $ "Window size: " <> displayShow (width, height)

  let ?window = snd !do
        allocate
          do traverse_ @[] GLFW.windowHint [ GLFW.WindowHint'ClientAPI GLFW.ClientAPI'NoAPI
                                           , GLFW.WindowHint'Resizable (not actuallyFullscreen)
                                           , GLFW.WindowHint'Decorated (not actuallyFullscreen)
                                           , GLFW.WindowHint'Floating actuallyFullscreen
                                           ]
             maybeM do throw GLFWWindowError
                     do \window -> do GLFW.setWindowPos window xPos yPos
                                      GLFW.setFramebufferSizeCallback window $ Just \_ _ _ ->
                                        writeIORef ?framebufferResized True
                                      pure window
                     do GLFW.createWindow width height !getProgName Nothing Nothing
          do GLFW.destroyWindow

  logDebug "Created window."
  pure Dict

initSurface :: (HasLogger, HasInstance, HasWindow) => ResIO (Dict HasSurface)
initSurface = do
  let ?surface = snd !do
        allocate
          do liftIO do
              surfacePtr <- malloc @SurfaceKHR
              result <- GLFW.createWindowSurface (instanceHandle ?instance) ?window nullPtr surfacePtr
              peek =<< catchVk (coerce @Int32 result, surfacePtr)
          do destroySurfaceKHR ?instance ?? Nothing

  logDebug "Created surface."
  pure Dict
  where
    catchVk = \case
      (SUCCESS, x) -> pure x
      (err    , _) -> throw . VkCouldn'tCreateSurface $ VulkanException err

initValidationLayers :: HasEnableValidationLayers => ResIO (Dict HasValidationLayers)
initValidationLayers = do
  let ?validationLayers = !do
        fmap V.fromList $ ?enableValidationLayers & bool (pure []) do
          case NE.nonEmpty $ filter (not . (snd !enumerateInstanceLayerProperties `hasLayer`)) layers of
            Nothing          -> pure layers
            Just unsupported -> throw $ VkValidationLayersNotSupported unsupported
  pure Dict
  where
    hasLayer props = V.elem ?? V.map layerName props
    layers = ["VK_LAYER_KHRONOS_validation"]

initPhysicalDevice :: (HasLogger, HasInstance, HasSurface, HasDebug)
                   => ResIO (Dict HasPhysicalDeviceRelated)
initPhysicalDevice = do
  dict <- enumeratePhysicalDevices ?instance >>= do
    snd >>> toList >>> NE.nonEmpty >>> maybe
      do throw VkNoPhysicalDevices
      \(toList -> physDevs) -> do
        let lds = length physDevs
        logDebug . (("Found " <> display lds <> plural " physical device" lds <> ": ") <>) .
          displayBytesUtf8 . B.intercalate ", " 
          =<< traverse (fmap deviceName <$> getPhysicalDeviceProperties) physDevs

        fmap ((\(Arg _ x) -> x) . getMax . sconcat) .
          fromMaybeM (throw VkNoSuitableDevices) . fmap NE.nonEmpty $
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
    score = do
      props <- getPhysicalDeviceProperties ?physicalDevice
      pure . sum @[] $ (\(p, points) -> [ points | p props ]) =<<
        [ ((PHYSICAL_DEVICE_TYPE_DISCRETE_GPU ==) . deviceType, 1000)
        ]

    findQueueFamilies :: (MonadResource m, HasLogger, HasPhysicalDevice)
                      => MaybeT m (Dict HasQueueFamilyIndices)
    findQueueFamilies = do
      props <- getPhysicalDeviceQueueFamilyProperties ?physicalDevice

      let logResult name queueIndex = logDebug $
            "Found " <> name <> " queue family (index " <> display queueIndex <> ") on " <>
            !devName <> "."

          findByFlag flag name = do
            Just queue <- pure $ fromIntegral <$>
              V.findIndex (\p -> queueFlags p .&. flag > zero) props
            logResult name queue
            pure queue

      let ?graphicsQueueFamily = !do findByFlag QUEUE_GRAPHICS_BIT "graphics"

      let indices = ZipList (toList props) *> [0..]
          surfaceSupport = getPhysicalDeviceSurfaceSupportKHR ?physicalDevice ?? ?surface
      let ?presentQueueFamily = !do joinMaybeT $ fmap fst . find snd <$> (traverse . traverseToSnd) surfaceSupport indices
      logResult "present" ?presentQueueFamily

      let ?computeQueueFamily = !do findByFlag QUEUE_COMPUTE_BIT "compute"

      pure Dict

    checkDeviceExtensionSupport :: (MonadIO m, HasPhysicalDevice, HasDebug) => m Bool
    checkDeviceExtensionSupport = do
      exts <- fmap extensionName . snd <$> enumerateDeviceExtensionProperties ?physicalDevice Nothing
      let isSupported = V.all (`elem` exts) deviceExtensions
      logDebug $ !devName <> " " <> bool "doesn't support" "supports" isSupported <>
                 " all extensions in " <> displayShow deviceExtensions <> "."
      pure isSupported

    querySwapchainSupport :: (MonadIO m, HasPhysicalDevice)
                          => MaybeT m (Dict HasSwapchainSupport)
    querySwapchainSupport = do
      let ?swapchainPresentModes = !do joinMaybeT $ NE.nonEmpty . toList . snd <$>
                                         getPhysicalDeviceSurfacePresentModesKHR ?physicalDevice ?surface
          ?swapchainFormats      = !do joinMaybeT $ NE.nonEmpty . toList . snd <$>
                                         getPhysicalDeviceSurfaceFormatsKHR ?physicalDevice ?surface
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
      logDebug $ "Using " <> displayShow ?msaaSamples <> " for MSAA on " <> !devName <> "."
      pure Dict

    devName :: (MonadIO m, HasPhysicalDevice) => m Utf8Builder
    devName = displayBytesUtf8 . deviceName <$> getPhysicalDeviceProperties ?physicalDevice

initDevice :: (HasLogger, HasPhysicalDeviceRelated, HasValidationLayers, HasDebug)
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
  let ?device = snd !do withDevice ?physicalDevice deviceCreateInfo Nothing allocate

  logDebug "Created logical device."
  pure Dict

initQueues :: (HasLogger, HasDevice, HasQueueFamilyIndices) => ResIO (Dict HasQueues)
initQueues = do
  let getQueue = getDeviceQueue ?device
  let ?graphicsQueue = !do getQueue ?graphicsQueueFamily 0
      ?presentQueue  = !do getQueue ?presentQueueFamily  0
      -- Is it better to use a compute queue that's different from the graphics
      -- queue? Not sure. Doing it for now though.
      ?computeQueue  = !do getQueue ?computeQueueFamily  1

  logDebug "Obtained device queues."
  pure Dict

deviceExtensions :: HasDebug => Vector ByteString
deviceExtensions =
  [ KHR_SHADER_NON_SEMANTIC_INFO_EXTENSION_NAME | ?enableDebug ] <>
  [ KHR_SWAPCHAIN_EXTENSION_NAME
  , KHR_STORAGE_BUFFER_STORAGE_CLASS_EXTENSION_NAME
  ]

initCommandPools :: (HasLogger, HasDevice, HasGraphicsQueueFamily, HasComputeQueueFamily)
                 => ResIO (Dict (HasGraphicsCommandPool, HasComputeCommandPool))
initCommandPools = do
  let graphicsCommandPoolInfo = zero{ queueFamilyIndex = ?graphicsQueueFamily
                                    } :: CommandPoolCreateInfo
  let ?graphicsCommandPool = snd !do withCommandPool ?device graphicsCommandPoolInfo Nothing allocate

  let computeCommandPoolInfo = zero{ queueFamilyIndex = ?computeQueueFamily
                                   } :: CommandPoolCreateInfo
  let ?computeCommandPool = snd !do withCommandPool ?device computeCommandPoolInfo Nothing allocate

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

  memoryTypeIndex <- maybe (throw VkNoSuitableMemoryType) (pure . fromIntegral . fst) memoryType

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
    bufs -> throw $ VkWrongNumberOfCommandBuffers 1 (fromIntegral $ length bufs)

initVertexBuffer :: (HasLogger, HasPhysicalDevice, HasDevice, HasGraphicsQueue, HasGraphicsCommandPool,
                     HasVertexBufferInfo, HasVertexData)
                 => ResIO (Dict HasVertexBuffer)
initVertexBuffer = do
  let stagingBufInfo = ?vertexBufferInfo{ usage = BUFFER_USAGE_TRANSFER_SRC_BIT
                                        , sharingMode = SHARING_MODE_EXCLUSIVE
                                        } :: BufferCreateInfo '[]
      stagingBufProps = MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. MEMORY_PROPERTY_HOST_COHERENT_BIT

  ((sBufKey, stagingBuffer), (sMemKey, stagingMemory)) <- constructBuffer stagingBufInfo stagingBufProps

  liftIO $ withMappedMemory ?device stagingMemory 0 ?vertexBufferInfo.size zero bracket \target -> do
    MkVertexData vertexData <- pure ?vertexData
    VS.unsafeWith (Sized.SomeSized vertexData) \(castPtr -> source) ->
      copyBytes target source $ fromIntegral ?vertexBufferInfo.size

  let vertBufProps = MEMORY_PROPERTY_DEVICE_LOCAL_BIT
  let ?vertexBuffer = snd . fst $ !do constructBuffer ?vertexBufferInfo vertBufProps

  copyBuffer ?graphicsCommandPool ?graphicsQueue stagingBuffer ?vertexBuffer ?vertexBufferInfo.size

  -- We don't need the staging buffer anymore
  traverse_ @[] release [sBufKey, sMemKey]

  logDebug "Created vertex buffer."
  pure Dict

-- TODO factor out common code with initVertexBuffer
initComputeStorageBuffers :: (HasLogger, HasPhysicalDevice, HasDevice, HasComputeCommandPool,
                              HasComputeQueue, HasComputeStorageData)
                          => ResIO (Dict HasComputeStorageBuffers)
initComputeStorageBuffers = do
  let ?computeStorageBuffers = !do
        for ?computeStorageData \(MkStorageData storageData) -> do
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

  logDebug "Created compute storage buffers."
  pure Dict

initGraphicsUniformBuffers :: (HasLogger, HasSurface, HasPhysicalDevice, HasDevice,
                               HasDesiredSwapchainImageNum)
                           => ResIO (Dict HasGraphicsUniformBuffers)
initGraphicsUniformBuffers = do
  surfCaps <- getPhysicalDeviceSurfaceCapabilitiesKHR ?physicalDevice ?surface

  -- We need at least as many buffers as we have images
  let numBuffers = max (fromIntegral surfCaps.minImageCount) (fromIntegral ?desiredSwapchainImageNum)
      bufInfo = zero{ size = graphicsUniformBufferSize
                    , usage = BUFFER_USAGE_UNIFORM_BUFFER_BIT
                    , sharingMode = SHARING_MODE_EXCLUSIVE
                    } :: BufferCreateInfo '[]
      properties = MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. MEMORY_PROPERTY_HOST_COHERENT_BIT
  let ?graphicsUniformBuffers = !do
        V.replicateM numBuffers do
          ((_, buffer), (_, memory)) <- constructBuffer bufInfo properties
          pure (buffer, memory)

  logDebug "Created graphics uniform buffers."

  pure Dict

initComputeUniformBuffer :: (HasLogger, HasPhysicalDevice, HasDevice)
                         => ResIO (Dict HasComputeUniformBuffer)
initComputeUniformBuffer = do
  let bufInfo = zero{ size = computeUniformBufferSize
                    , usage = BUFFER_USAGE_UNIFORM_BUFFER_BIT
                    , sharingMode = SHARING_MODE_EXCLUSIVE
                    } :: BufferCreateInfo '[]
      properties = MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. MEMORY_PROPERTY_HOST_COHERENT_BIT
  ((_, buffer), (_, memory)) <- constructBuffer bufInfo properties
  let ?computeUniformBuffer = (buffer, memory)

  logDebug "Created compute uniform buffer."

  pure Dict

initGraphicsDescriptorSetLayout :: (HasDevice, HasGraphicsDescriptorSetLayoutInfo)
                                => ResIO (Dict HasGraphicsDescriptorSetLayout)
initGraphicsDescriptorSetLayout = do
  let ?graphicsDescriptorSetLayout = snd !do
        withDescriptorSetLayout ?device ?graphicsDescriptorSetLayoutInfo Nothing allocate
  pure Dict

initComputeDescriptorSetLayout :: (HasDevice, HasComputeDescriptorSetLayoutInfo)
                               => ResIO (Dict HasComputeDescriptorSetLayouts)
initComputeDescriptorSetLayout = do
  let ?computeDescriptorSetLayouts = !do
        forM ?computeDescriptorSetLayoutInfo \layoutInfo ->
          snd <$> withDescriptorSetLayout ?device layoutInfo Nothing allocate
  pure Dict

initVertexStorageBuffer :: HasVertexBuffer => ResIO (Dict HasVertexStorageBuffer)
initVertexStorageBuffer = do
  let ?vertexStorageBuffer = ?vertexBuffer
  pure Dict

initComputeFences :: HasDevice => ResIO (Dict HasComputeFences)
initComputeFences = do
  let ?computeFences = !do
        both snd <$> bisequence
          (dupe $ withFence ?device zero{flags = FENCE_CREATE_SIGNALED_BIT} Nothing allocate)
  pure Dict

initSyncs :: (HasLogger, HasDevice) => ResIO (Dict HasSyncs)
initSyncs = do
  (imageAvailable, renderFinished) <- bisequence . dupe . Sized.replicateM $
    snd <$> withSemaphore ?device zero Nothing allocate
  let ?imageAvailable = imageAvailable
      ?renderFinished = renderFinished
      ?inFlight = !do
        Sized.replicateM $
          snd <$> withFence ?device zero{flags = FENCE_CREATE_SIGNALED_BIT} Nothing allocate

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
