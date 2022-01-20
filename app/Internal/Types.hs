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
import Control.Monad.Trans.Resource (ReleaseKey, MonadResource)

type MaxFramesInFlight = 2

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

data WindowSize = MkWindowSize { windowWidth  :: Natural
                               , windowHeight :: Natural
                               }
makeRioClassy ''WindowSize

data Config = MkConfig { windowSize             :: WindowSize
                       , fullscreen             :: Bool
                       , monitorIndex           :: Natural
                       , enableValidationLayers :: Bool
                       }
makeRioClassy ''Config

data App = MkApp { logFunc :: LogFunc
                 , config  :: Config
                 }
makeRioClassy ''App

data Queues = MkQueues { graphicsQueue :: Queue
                       , presentQueue  :: Queue
                       }
makeRioClassy ''Queues

data SwapchainRelated = MkSwapchainRelated { swapchainFormat        :: SurfaceFormatKHR
                                           , swapchainExtent        :: IORef Extent2D
                                           , swapchain              :: MResource SwapchainKHR
                                           , renderPass             :: MResource RenderPass
                                           , graphicsPipelineLayout :: MResource PipelineLayout
                                           , graphicsPipeline       :: MResource Pipeline
                                           }
makeRioClassy ''SwapchainRelated

data QueueFamilyIndices = MkQueueFamilyIndices { graphicsQueueFamily :: Word32
                                               , presentQueueFamily  :: Word32
                                               }
makeRioClassy ''QueueFamilyIndices

data SwapchainSupport = MkSwapchainSupport { swapchainCapabilities :: SurfaceCapabilitiesKHR
                                           , swapchainFormats      :: NonEmpty SurfaceFormatKHR
                                           , swapchainPresentModes :: NonEmpty PresentModeKHR
                                           }
makeRioClassy ''SwapchainSupport

data GraphicsResources = MkGraphicsResources { window             :: GLFW.Window
                                             , inst               :: Instance
                                             , device             :: Device
                                             , queues             :: Queues
                                             , queueFamilyIndices :: QueueFamilyIndices
                                             , surface            :: SurfaceKHR
                                             , swapchainSupport   :: SwapchainSupport
                                             }
makeRioClassy ''GraphicsResources

type SyncVector = Sized.Vector MaxFramesInFlight
data Syncs = MkSyncs { imageAvailable :: SyncVector Semaphore
                     , renderFinished :: SyncVector Semaphore
                     , inFlight       :: SyncVector Fence
                     }
makeRioClassy ''Syncs

data GraphicsApp = MkGraphicsApp { app :: App
                                 , graphicsResources :: GraphicsResources
                                 }
makeRioClassy ''GraphicsApp

data SwapchainApp = MkSwapchainApp { graphicsApp      :: GraphicsApp
                                   , swapchainRelated :: SwapchainRelated
                                   , commandPool      :: CommandPool
                                   }
makeRioClassy ''SwapchainApp

data Boxticle = MkBoxticle { swapchainApp      :: SwapchainApp
                           , imageRelateds     :: Vector ImageRelated
                           , syncs             :: Syncs
                           }
makeRioClassy ''Boxticle

type MonadRR env m = (MonadResource m, MonadReader env m)
