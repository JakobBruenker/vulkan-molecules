{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_HADDOCK not-home #-}

module Internal.Types where

import RIO
import RIO.ByteString qualified as B
import RIO.Text qualified as T

import Data.Vector.Sized qualified as Sized

import Vulkan hiding (Display)

import TH
import Graphics.UI.GLFW qualified as GLFW
import Control.Monad.Trans.Resource (ReleaseKey, release)
import Data.Coerce (coerce, Coercible)

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

-- a resource that must be allocated and released
data Resource f a = MkResource { releaseKey :: f ReleaseKey
                               , resource   :: a
                               }

-- a mutable resource
type MResource a = IORef (Resource Identity a)

-- a collection of mutable resources
type MResources a = IORef (Resource [] a)

mkMResourceGeneric :: (Coercible key (f ReleaseKey), MonadIO m)
                   => (key, a) -> m (IORef (Resource f a))
mkMResourceGeneric = newIORef . uncurry MkResource . coerce

readResGeneric :: MonadIO m => IORef (Resource f a) -> m a
readResGeneric = fmap resource . readIORef

-- | Releases the old resource(s) and registers the new one.
writeResGeneric :: (Coercible key (f ReleaseKey), Traversable f, MonadIO m)
                => IORef (Resource f a) -> (key, a) -> m ()
writeResGeneric res new = do
  traverse_ release . releaseKey =<< readIORef res
  writeIORef res . uncurry MkResource $ coerce new

mkMResource :: MonadIO m => (ReleaseKey, a) -> m (MResource a)
mkMResource = mkMResourceGeneric

readRes :: MonadIO m => MResource a -> m a
readRes = readResGeneric

-- | Releases the old resource and registers the new one.
writeRes :: MonadIO m => MResource a -> (ReleaseKey, a) -> m ()
writeRes = writeResGeneric

mkMResources :: MonadIO m => ([ReleaseKey], a) -> m (MResources a)
mkMResources = mkMResourceGeneric

readRess :: MonadIO m => MResources a -> m a
readRess = readResGeneric

-- | Releases the old resources and registers the new one.
writeRess :: MonadIO m => MResources a -> ([ReleaseKey], a) -> m ()
writeRess = writeResGeneric

data ImageRelated = MkImageRelated { image         :: Image
                                   , imageInFlight :: IORef (Maybe Fence)
                                   , imageView     :: ImageView
                                   , framebuffer   :: Framebuffer
                                   , commandBuffer :: CommandBuffer
                                   }
makeRegularClassy ''ImageRelated

makeRegularClassy ''SurfaceFormatKHR

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

type HasSwapchainFormat        = ?swapchainFormat        :: SurfaceFormatKHR
type HasSwapchainExtent        = ?swapchainExtent        :: IORef Extent2D
type HasSwapchain              = ?swapchain              :: MResource SwapchainKHR
type HasRenderPass             = ?renderPass             :: MResource RenderPass
type HasGraphicsPipelineLayout = ?graphicsPipelineLayout :: MResource PipelineLayout
type HasGraphicsPipeline       = ?graphicsPipeline       :: MResource Pipeline
type HasSwapchainRelated = ( HasSwapchainFormat
                           , HasSwapchainExtent
                           , HasSwapchain
                           , HasRenderPass
                           , HasGraphicsPipelineLayout
                           , HasGraphicsPipeline
                           )

type HasGraphicsQueueFamily = ?graphicsQueueFamily :: Word32
type HasPresentQueueFamily  = ?presentQueueFamily  :: Word32
type HasQueueFamilyIndices = ( HasGraphicsQueueFamily
                             , HasPresentQueueFamily
                             )

type HasSwapchainCapabilities = ?swapchainCapabilities :: SurfaceCapabilitiesKHR
type HasSwapchainFormats      = ?swapchainFormats      :: NonEmpty SurfaceFormatKHR
type HasSwapchainPresentModes = ?swapchainPresentModes :: NonEmpty PresentModeKHR
type HasSwapchainSupport = ( HasSwapchainCapabilities
                           , HasSwapchainFormats
                           , HasSwapchainPresentModes
                           )

type HasPhysicalDevice = ?physicalDevice :: PhysicalDevice
type HasPhysicalDeviceRelated = ( HasPhysicalDevice
                                , HasQueueFamilyIndices
                                , HasSwapchainSupport
                                )

type HasGLFW             = ?glfw             :: GLFWToken
type HasWindow           = ?window           :: GLFW.Window
type HasInstance         = ?instance         :: Instance
type HasValidationLayers = ?validationLayers :: Vector ByteString
type HasDevice           = ?device           :: Device
type HasSurface          = ?surface          :: SurfaceKHR
type HasGraphicsResources = ( HasGLFW
                            , HasWindow
                            , HasInstance
                            , HasValidationLayers
                            , HasDevice
                            , HasSurface
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

type HasCommandPool              = ?commandPool              :: CommandPool
type HasImageRelateds            = ?imageRelateds            :: MResources (Vector ImageRelated)
type HasVulkanResources =
  ( HasCommandPool,
    HasImageRelateds,
    HasGraphicsResources,
    HasSwapchainRelated,
    HasSyncs
  )

type HasApp =
  ( HasConfig,
    HasLogger,
    HasVulkanResources
  )
