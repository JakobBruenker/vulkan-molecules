{-# OPTIONS_HADDOCK not-home #-}

module Internal.Types where

import RIO
import RIO.ByteString qualified as B
import RIO.Text qualified as T

import Data.Vector.Sized qualified as Sized

import Vulkan hiding ( Display
                     , Win32KeyedMutexAcquireReleaseInfoNV(releaseKeys)
                     , Win32KeyedMutexAcquireReleaseInfoKHR(releaseKeys)
                     )

import Graphics.UI.GLFW qualified as GLFW
import Control.Monad.Trans.Resource (ReleaseKey, release)

type MaxFramesInFlight = 2

data Dict c = c => Dict

data AppException
  = GLFWInitError
  | GLFWWindowError
  | GLFWVulkanNotSupportedError
  | VkValidationLayersNotSupported (NonEmpty ByteString)
  | VkNoPhysicalDevicesError
  | VkNoSuitableDevicesError
  | VkWrongNumberOfGraphicsPipelines ("expected" ::: Natural) ("actual" ::: Natural)
  | VkCommandBufferIndexOutOfRange

instance Exception AppException

instance Show AppException where
  show = T.unpack . textDisplay

instance Display AppException where
  display = \case
    GLFWInitError -> "Failed to initialize GLFW."
    GLFWWindowError -> "Failed to create window."
    GLFWVulkanNotSupportedError -> "GLFW failed to find Vulkan loader or ICD."
    VkValidationLayersNotSupported ls ->
      "Requested validation layer" <> num <> " not supported: " <>
        (displayBytesUtf8 . B.intercalate ", " . toList) ls <> "."
      where num | _ :| [] <- ls = " is"
                | otherwise     = "s are"
    VkNoPhysicalDevicesError -> "No physical devices found."
    VkNoSuitableDevicesError -> "No suitable devices found."
    VkWrongNumberOfGraphicsPipelines expected actual ->
      "Wrong number of graphics pipelines was created: Expected " <>
      displayShow expected <> " but got " <> displayShow actual <> "."
    VkCommandBufferIndexOutOfRange -> "Vulkan requested a command buffer with" <>
      " a higher index than available."

data Options = MkOptions { optWidth            :: Natural
                         , optHeight           :: Natural
                         , optFullscreen       :: Bool
                         , optMonitorIndex     :: Natural
                         , optVerbose          :: Bool
                         , optValidationLayers :: Bool
                         }

-- abstract type to keep track of whether GLFW is initialized
data GLFWToken = UnsafeMkGLFWToken

data ShouldRecreateSwapchain = Don'tRecreate
                             | PleaseRecreate
                             deriving Eq

-- a collection of resources that must be allocated and released
data Resources a = MkResource { releaseKeys :: [ReleaseKey]
                              , resources   :: a
                              }

-- a collection of mutable resources
type MResources a = IORef (Resources a)

mkMResources :: MonadIO m => ([ReleaseKey], a) -> m (MResources a)
mkMResources = newIORef . uncurry MkResource

readRes :: MonadIO m => MResources a -> m a
readRes = fmap resources . readIORef

-- | Releases the old resources and registers the new one.
--
-- The second argument is an action rather than a value to make sure the
-- resources can be freed before the action is run.
writeRes :: MonadIO m => MResources a -> m ([ReleaseKey], a) -> m ()
writeRes res new = do
  traverse_ release . releaseKeys =<< readIORef res
  writeIORef res . uncurry MkResource =<< new

data ImageRelated = MkImageRelated { image         :: Image
                                   , imageInFlight :: IORef (Maybe Fence)
                                   , imageView     :: ImageView
                                   , framebuffer   :: Framebuffer
                                   , commandBuffer :: CommandBuffer
                                   }

data Mutables = MkMutables { imageRelateds          :: Vector ImageRelated
                           , swapchainFormat        :: Format
                           , swapchainExtent        :: Extent2D
                           , swapchain              :: SwapchainKHR
                           , renderPass             :: RenderPass
                           , graphicsPipelineLayout :: PipelineLayout
                           , graphicsPipeline       :: Pipeline
                           }

type HasLogger = ?logFunc :: LogFunc

type HasWindowWidth  = ?windowWidth  :: Natural
type HasWindowHeight = ?windowHeight :: Natural
type HasWindowSize = ( HasWindowWidth
                     , HasWindowHeight
                     )

type HasFullscreen             = ?fullscreen             :: Bool
type HasMonitorIndex           = ?monitorIndex           :: Natural
type HasEnableValidationLayers = ?enableValidationLayers :: Bool
type HasConfig = ( HasFullscreen
                 , HasMonitorIndex
                 , HasEnableValidationLayers
                 , HasWindowSize
                 )

type HasGraphicsQueue = ?graphicsQueue :: Queue
type HasPresentQueue  = ?presentQueue :: Queue
type HasQueues = ( HasGraphicsQueue
                 , HasPresentQueue
                 )

type HasGraphicsQueueFamily = ?graphicsQueueFamily :: Word32
type HasPresentQueueFamily  = ?presentQueueFamily  :: Word32
type HasQueueFamilyIndices = ( HasGraphicsQueueFamily
                             , HasPresentQueueFamily
                             )

type HasSwapchainFormats      = ?swapchainFormats      :: NonEmpty SurfaceFormatKHR
type HasSwapchainPresentModes = ?swapchainPresentModes :: NonEmpty PresentModeKHR
type HasSwapchainSupport = ( HasSwapchainFormats
                           , HasSwapchainPresentModes
                           )

type HasPhysicalDevice = ?physicalDevice :: PhysicalDevice
type HasPhysicalDeviceRelated = ( HasPhysicalDevice
                                , HasQueueFamilyIndices
                                , HasSwapchainSupport
                                )

type HasGLFW               = ?glfw               :: GLFWToken
type HasWindow             = ?window             :: GLFW.Window
type HasFramebufferResized = ?framebufferResized :: IORef Bool
type HasInstance           = ?instance           :: Instance
type HasValidationLayers   = ?validationLayers   :: Vector ByteString
type HasDevice             = ?device             :: Device
type HasSurface            = ?surface            :: SurfaceKHR
type HasCommandPool        = ?commandPool        :: CommandPool
type HasGraphicsResources = ( HasGLFW
                            , HasWindow
                            , HasFramebufferResized
                            , HasInstance
                            , HasValidationLayers
                            , HasDevice
                            , HasSurface
                            , HasCommandPool
                            , HasQueues
                            , HasPhysicalDeviceRelated
                            )

type SyncVector = Sized.Vector MaxFramesInFlight

type HasImageAvailable = ?imageAvailable :: SyncVector Semaphore
type HasRenderFinished = ?renderFinished :: SyncVector Semaphore
type HasInFlight       = ?inFlight       :: SyncVector Fence
type HasSyncs = ( HasImageAvailable
                , HasRenderFinished
                , HasInFlight
                )

type HasMutables = ?mutables :: MResources Mutables
type HasVulkanResources =
  ( HasMutables
  , HasGraphicsResources
  , HasSyncs
  )

type HasApp =
  ( HasConfig
  , HasLogger
  , HasVulkanResources
  )
