{-# LANGUAGE OverloadedLists #-}

module Graphics.Initialize where

import RIO hiding (logDebug, logInfo, logWarn, logError)
import RIO.ByteString qualified as B
import RIO.NonEmpty qualified as NE
import RIO.Vector qualified as V

import Control.Lens ((??), ix)
import Control.Monad.Extra (fromMaybeM, ifM, maybeM)
import Data.Bits (Bits((.&.), (.|.)))
import System.Environment (getProgName)
import Control.Applicative (ZipList(..))
import Control.Monad.Trans.Maybe (runMaybeT, MaybeT)
import Control.Monad.Trans.Resource (allocate, ResIO, MonadResource)
import Data.Coerce (coerce)
import Data.Foldable (find)
import Data.List (nub)
import Foreign (malloc, nullPtr, Storable(peek), freeHaskellFunPtr)


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
import Data.List.NonEmpty.Extra (maximumOn1)

initGLFW :: HasLogger => ResIO (Dict HasGLFW)
initGLFW = do
  (_, glfwToken) <- allocate
    do liftIO $ ifM GLFW.init do pure UnsafeMkGLFWToken
                              do throwIO GLFWInitError
    do const $ liftIO GLFW.terminate
  let ?glfw = glfwToken

  logDebug "Initialized GLFW."
  pure Dict

debugCallback :: HasLogger => FN_vkDebugUtilsMessengerCallbackEXT
debugCallback severity msgType callbackData _userData = do
  DebugUtilsMessengerCallbackDataEXT{ messageIdName
                                    , messageIdNumber
                                    , message
                                    , queueLabels
                                    , cmdBufLabels
                                    , objects
                                    } <- peekCStruct callbackData
  let logFun = case severity of
        DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT -> logDebug
        DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT    -> logInfo
        DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT -> logWarn
        DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT   -> logError
        _                                            -> logDebug

      msgTypeStr = case msgType of
        DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT     -> "General"
        DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT  -> "Validation"
        DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT -> "Performance"
        _ -> "Unknown category"

  logFun $ "Validation layer message (" <> msgTypeStr <> "):\n"
        <> "ID: " <> display messageIdNumber <> " - "
        <> maybe "Unknown" displayBytesUtf8 messageIdName <> "\n"
        <> displayBytesUtf8 message <> "\n"
        <> "Active queue labels: " <> displayShow queueLabels <> "\n"
        <> "Active command buffer labels: " <> displayShow cmdBufLabels <> "\n"
        <> "Related objects: " <> displayShow objects

  -- Always return FALSE to indicate that the triggering Vulkan call should not
  -- be aborted.
  pure FALSE

-- *Sigh* this doesn't actually work, you get
--   "schedule: re-entered unsafely"
-- at runtime:
-- The problem: If you entered C-land via an unsafe foreign function call, you
-- *cannot* use Haskell callbacks from there.
-- However: Maybe we can use the callback to give the data to a global pointer,
-- and then periodically check the pointer in Haskell. That's somewhat
-- unsatisfying though, because it'd really be preferable to get logs directly
-- rather than after calling. It might also be possible to somehow set up a
-- separate thread, and have it wait to be signalled by the callback written in C.
foreign import ccall "wrapper"
  createDebugCallback :: FN_vkDebugUtilsMessengerCallbackEXT -> IO PFN_vkDebugUtilsMessengerCallbackEXT

initDebugMessenger :: (HasLogger, HasInstance) => ResIO ()
initDebugMessenger = do
  (_, callback) <- allocate (createDebugCallback debugCallback) freeHaskellFunPtr
  let debugMsngrInfo = zero{ messageSeverity = DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT
                                           .|. DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT
                                           .|. DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT
                                           .|. DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT
                           , messageType = DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT
                                       .|. DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT
                                       .|. DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT
                           , pfnUserCallback = callback
                           }
  void $ withDebugUtilsMessengerEXT ?instance debugMsngrInfo Nothing allocate

initInstance :: (HasLogger, HasValidationLayers) => ResIO (Dict HasInstance)
initInstance = do
  unlessM (liftIO GLFW.vulkanSupported) $ throwIO GLFWVulkanNotSupportedError
  logDebug "Verified GLFW Vulkan support."

  enabledExtensionNames <- V.fromList . mappend
    [EXT_DEBUG_UTILS_EXTENSION_NAME | not $ null ?validationLayers] <$> do
      liftIO GLFW.getRequiredInstanceExtensions >>= liftIO . mapM B.packCString
  logDebug $ "Required extensions for GLFW: " <> displayShow enabledExtensionNames

  let applicationInfo = Just (zero{apiVersion = MAKE_API_VERSION 1 0 0} :: ApplicationInfo)
      instanceCreateInfo = zero{ applicationInfo
                               , enabledExtensionNames
                               , enabledLayerNames = ?validationLayers
                               }

  (_, inst) <- withInstance instanceCreateInfo Nothing allocate
  let ?instance = inst

  logDebug "Created instance."

  initDebugMessenger

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

initializeVulkan :: (HasLogger, HasConfig, HasGraphicsPipelineLayoutInfo)
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
  Dict <- initMutables
  Dict <- initSyncs

  setupGraphicsCommands $> Dict
