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
import Control.Monad.Trans.Resource (ReleaseKey)

type MaxFramesInFlight = 2

data Dict c = c => Dict

data AppException
  = GLFWInitError
  | GLFWWindowError
  | GLFWVulkanNotSupportedError
  | VkGenericError Result
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
    VkGenericError r -> "Vulkan returned error: " <> displayShow r
    VkValidationLayersNotSupported ls ->
      "Requested validation layer" <> num <> " not supported: " <>
        (displayBytesUtf8 . B.intercalate ", " . toList) ls <> "."
      where num = case ls of
              _ :| [] -> " is"
              _       -> "s are"
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

-- a mutable resource
type MResource a = IORef (Resource a)

data Resource a = MkResource { releaseKey :: ReleaseKey
                             , resource   :: a
                             }
makeRegularClassy ''Resource

data ImageRelated = MkImageRelated { image         :: IORef Image
                                   , imageInFlight :: IORef (Maybe Fence)
                                   , imageView     :: MResource ImageView
                                   , framebuffer   :: MResource Framebuffer
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

type HasCommandPool   = ?commandPool   :: CommandPool
type HasImageRelateds = ?imageRelateds :: Vector ImageRelated
type HasVulkanResources = ( HasCommandPool
                          , HasImageRelateds
                          , HasGraphicsResources
                          , HasSwapchainRelated
                          , HasSyncs
                          )

type HasApp = ( HasConfig
              , HasLogger
              , HasVulkanResources
              )
