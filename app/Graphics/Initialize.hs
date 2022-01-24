{-# LANGUAGE OverloadedLists #-}

module Graphics.Initialize where

import RIO hiding (logDebug, logInfo, logWarn, logError)
import RIO.ByteString qualified as B
import RIO.NonEmpty qualified as NE
import RIO.Vector qualified as V
import RIO.Vector.Storable.Partial qualified as VS
import RIO.Vector.Storable.Unsafe qualified as VS

import Control.Lens ((??), ix)
import Control.Monad.Extra (fromMaybeM, ifM, maybeM)
import Data.Bits (Bits((.&.)), testBit, (.|.), xor)
import System.Environment (getProgName)
import Control.Applicative (ZipList(..))
import Control.Monad.Trans.Maybe (runMaybeT, MaybeT)
import Control.Monad.Trans.Resource (allocate, ResIO, MonadResource, ReleaseKey, release)
import Data.Coerce (coerce)
import Data.Foldable (find)
import Data.List (nub)
import Foreign (malloc, nullPtr, Storable (peek, sizeOf), copyBytes, castPtr)
import Foreign.Storable.Tuple ()
import Data.List.NonEmpty.Extra (maximumOn1)
import Control.Lens.Combinators (ifind)


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

import Graphics.Internal.Types(GLFWToken(UnsafeMkGLFWToken))

import Graphics.Mutables
import Utils
import Graphics.Utils
import Graphics.Types
import qualified Data.Vector.Storable.Sized as Sized

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

  let applicationInfo = Just (zero{apiVersion = MAKE_API_VERSION 1 0 0} :: ApplicationInfo)
      instanceCreateInfo = zero{ applicationInfo
                               , enabledExtensionNames
                               , enabledLayerNames = ?validationLayers
                               }

  (_, inst) <- withInstance instanceCreateInfo Nothing allocate
  let ?instance = inst

  logDebug "Created instance."
  pure Dict

-- FIXME: when you have two monitors your focus is on the smaller monitor, a
-- fullscreen window will be too small (with X11).
initWindow :: (HasLogger, HasConfig, HasGLFW) => ResIO (Dict (HasWindow, HasFramebufferResized))
initWindow = do
  !_ <- pure ?glfw -- making sure GLFW really is initialized

  framebufferResized <- newIORef False

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
        logDebug $ "Found " <> display lds <> plural " physical device" lds <> "."

        fmap (fst . maximumOn1 snd) $
          fromMaybeM (throwIO VkNoSuitableDevices) . fmap NE.nonEmpty $
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

-- TODO: combine buffers and allocations into single buffers and use offsets to
-- bind, where it makes sense
constructBuffer :: (HasPhysicalDevice, HasDevice)
                => BufferCreateInfo '[] -> MemoryPropertyFlags
                -> ResIO ((ReleaseKey, Buffer), (ReleaseKey, DeviceMemory))
constructBuffer info properties = do
  (bufferKey, buffer) <- withBuffer ?device info Nothing allocate

  MemoryRequirements{ size = allocationSize
                    , memoryTypeBits
                    } <- getBufferMemoryRequirements ?device buffer
  PhysicalDeviceMemoryProperties{memoryTypes} <- getPhysicalDeviceMemoryProperties ?physicalDevice

  let memoryType = ifind ?? memoryTypes $ \i MemoryType{propertyFlags} ->
        testBit memoryTypeBits i &&
        -- check whether all properties are set
        ((propertyFlags `xor` properties) .&. properties == zero)

  memoryTypeIndex <- maybe (throwIO VkNoSuitableMemoryType) (pure . fromIntegral . fst) memoryType

  let allocInfo = zero{allocationSize , memoryTypeIndex}
  (memoryKey, memory) <- withMemory ?device allocInfo Nothing allocate

  bindBufferMemory ?device buffer memory 0

  pure ((bufferKey, buffer), (memoryKey, memory))

copyBuffer :: (MonadUnliftIO m, HasDevice, HasCommandPool)
           => Queue -> "src" ::: Buffer -> "dst" ::: Buffer -> DeviceSize -> m ()
copyBuffer queue src dst size = do
  let allocInfo = zero{ level = COMMAND_BUFFER_LEVEL_PRIMARY
                      , commandPool = ?commandPool
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

initVertexBuffer :: (HasLogger, HasPhysicalDevice, HasDevice, HasGraphicsQueue, HasCommandPool,
                     HasVertexBufferInfo, HasVertexData)
                 => ResIO (Dict HasVertexBuffer)
initVertexBuffer = do
  let stagingBufInfo = ?vertexBufferInfo{ usage = BUFFER_USAGE_TRANSFER_SRC_BIT
                                        , sharingMode = SHARING_MODE_EXCLUSIVE
                                        } :: BufferCreateInfo '[]
      stagingBufProps = MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. MEMORY_PROPERTY_HOST_COHERENT_BIT
      BufferCreateInfo{size = bufferSize} = ?vertexBufferInfo

  ((sBufKey, stagingBuffer), (sMemKey, stagingMemory)) <-
    constructBuffer stagingBufInfo stagingBufProps

  liftIO $ withMappedMemory ?device stagingMemory 0 bufferSize zero bracket \target -> do
    MkVertexData vertexData <- pure ?vertexData
    VS.unsafeWith (Sized.SomeSized vertexData) \(castPtr -> source) ->
      copyBytes target source $ Sized.length vertexData * sizeOf (VS.head $ Sized.SomeSized vertexData)

  let vertBufProps = MEMORY_PROPERTY_DEVICE_LOCAL_BIT
  ((_, vertexBuffer), _) <- constructBuffer ?vertexBufferInfo vertBufProps
  let ?vertexBuffer = vertexBuffer

  copyBuffer ?graphicsQueue stagingBuffer vertexBuffer bufferSize

  -- We don't need the staging buffer anymore
  traverse_ @[] release [sBufKey, sMemKey]

  logDebug "Created vertex buffer."
  pure Dict

initUniformBuffers :: (HasLogger, HasSurface, HasPhysicalDevice, HasDevice, HasUniformBufferSize,
                       HasDesiredSwapchainImageNum)
                   => ResIO (Dict HasUniformBuffers)
initUniformBuffers = do
  SurfaceCapabilitiesKHR{minImageCount} <-
    getPhysicalDeviceSurfaceCapabilitiesKHR ?physicalDevice ?surface

  -- We need at least as many buffers as we have images
  let numBuffers = max (fromIntegral minImageCount) (fromIntegral ?desiredSwapchainImageNum)
      bufInfo = zero{ size = ?uniformBufferSize
                    , usage = BUFFER_USAGE_UNIFORM_BUFFER_BIT
                    , sharingMode = SHARING_MODE_EXCLUSIVE
                    } :: BufferCreateInfo '[]
      properties = MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. MEMORY_PROPERTY_HOST_COHERENT_BIT
  buffers <- V.replicateM numBuffers do
    ((_, buffer), (_, memory)) <- constructBuffer bufInfo properties
    pure (buffer, memory)
  let ?uniformBuffers = buffers

  logDebug "Created uniform buffers."

  pure Dict

initDescriptorSetLayout :: (HasDevice, HasDescriptorSetLayoutInfo)
                        => ResIO (Dict HasDescriptorSetLayout)
initDescriptorSetLayout = do
  (_, layout) <- withDescriptorSetLayout ?device ?descriptorSetLayoutInfo Nothing allocate
  let ?descriptorSetLayout = layout
  pure Dict

initializeVulkan :: (HasLogger, HasConfig, HasShaderPaths, HasVulkanConfig)
                 => (HasVulkanResources => ResIO ()) -> ResIO (Dict HasVulkanResources)
initializeVulkan setupGraphicsCommands = do
  Dict <- initGLFW
  Dict <- initValidationLayers
  Dict <- initWindow
  Dict <- initInstance
  Dict <- initSurface
  Dict <- initPhysicalDevice
  Dict <- initDevice
  Dict <- initQueues
  Dict <- initCommandPool
  Dict <- initVertexBuffer
  Dict <- initUniformBuffers
  Dict <- initDescriptorSetLayout
  Dict <- initMutables
  Dict <- initSyncs

  setupGraphicsCommands $> Dict
