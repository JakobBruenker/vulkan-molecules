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
import Data.Bits (Bits((.&.)), testBit, (.|.))
import System.Environment (getProgName)
import Control.Applicative (ZipList(..))
import Control.Monad.Trans.Maybe (runMaybeT, MaybeT)
import Control.Monad.Trans.Resource (allocate, ResIO, MonadResource)
import Data.Coerce (coerce)
import Data.Foldable (find)
import Data.List (nub)
import Foreign (malloc, nullPtr, Storable (peek, sizeOf), copyBytes, castPtr)
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

import Internal.Types(GLFWToken(UnsafeMkGLFWToken))

import Graphics.Mutables
import Utils
import Types

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

initVertexBuffer :: (HasLogger, HasPhysicalDevice, HasDevice, HasVertexBufferInfo, HasVertexData)
                 => ResIO (Dict HasVertexBuffer)
initVertexBuffer = do
  (_, vertexBuffer) <- withBuffer ?device ?vertexBufferInfo Nothing allocate
  let ?vertexBuffer = vertexBuffer

  MemoryRequirements{ size = allocationSize
                    , memoryTypeBits
                    } <- getBufferMemoryRequirements ?device ?vertexBuffer
  PhysicalDeviceMemoryProperties{memoryTypes} <- getPhysicalDeviceMemoryProperties ?physicalDevice

  let properties = MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. MEMORY_PROPERTY_HOST_COHERENT_BIT
      memoryType = ifind ?? memoryTypes $ \i MemoryType{propertyFlags} ->
        testBit memoryTypeBits i && (propertyFlags .&. properties > zero)

  memoryTypeIndex <- maybe (throwIO VkNoSuitableMemoryType) (pure . fromIntegral . fst) memoryType

  let allocInfo = zero{allocationSize , memoryTypeIndex}
  (_, memory) <- withMemory ?device allocInfo Nothing allocate

  bindBufferMemory ?device ?vertexBuffer memory 0

  let BufferCreateInfo{size = bufferSize} = ?vertexBufferInfo

  liftIO $ withMappedMemory ?device memory 0 bufferSize zero bracket \target ->
    VS.unsafeWith ?vertexData \(castPtr -> source) -> do
      copyBytes target source $ V.length ?vertexData * sizeOf (VS.head ?vertexData)
      -- flushMappedMemoryRanges ?device [MappedMemoryRange{memory, offset = 0, size = WHOLE_SIZE}]

  logDebug "Created vertex buffer."
  pure Dict

initializeVulkan :: ( HasLogger, HasConfig, HasGraphicsPipelineLayoutInfo
                    , HasVertexBufferInfo, HasVertexInputInfo, HasVertexData)
                 => (HasVulkanResources => ResIO ()) -> ResIO (Dict HasVulkanResources)
initializeVulkan setupGraphicsCommands = do
  Dict <- initGLFW
  Dict <- initValidationLayers
  Dict <- initWindow
  Dict <- initInstance
  Dict <- initSurface
  Dict <- initPhysicalDevice
  Dict <- initDevice
  Dict <- initVertexBuffer
  Dict <- initQueues
  Dict <- initCommandPool
  Dict <- initMutables
  Dict <- initSyncs

  setupGraphicsCommands $> Dict