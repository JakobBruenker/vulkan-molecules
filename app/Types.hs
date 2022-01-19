{-# LANGUAGE TemplateHaskell #-}

module Types where

import RIO
import RIO.ByteString qualified as B
import RIO.Text qualified as T

import Vulkan hiding (Display)

import TH
import Graphics.UI.GLFW qualified as GLFW

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
                         , optVerbose          :: Bool
                         , optValidationLayers :: Bool
                         }

-- To keep track of whether GLFW is initialized
data GLFWToken = UnsafeMkGLFWToken

data BufferCollection = MkBufferCollection { image         :: Image
                                           , imageView     :: ImageView
                                           , framebuffer   :: Framebuffer
                                           , commandBuffer :: CommandBuffer
                                           }

data WindowSize = MkWindowSize { windowWidth  :: Natural
                               , windowHeight :: Natural
                               }
makeRioClassy ''WindowSize

data Config = MkConfig { windowSize             :: WindowSize
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

data SwapchainDetails = MkSwapchainDetails { swapchainFormat :: SurfaceFormatKHR
                                           , swapchainExtent :: Extent2D
                                           , swapchain       :: SwapchainKHR
                                           }
makeRioClassy ''SwapchainDetails

data QueueFamilyIndices = MkQueueFamilyIndices { graphicsQueueFamily :: Word32
                                               , presentQueueFamily  :: Word32
                                               }
makeRioClassy ''QueueFamilyIndices

data GraphicsResources = MkGraphicsResources { window             :: GLFW.Window
                                             , inst               :: Instance
                                             , device             :: Device
                                             , queues             :: Queues
                                             , queueFamilyIndices :: QueueFamilyIndices
                                             , surface            :: SurfaceKHR
                                             , swapchainDetails   :: SwapchainDetails
                                             }
makeRioClassy ''GraphicsResources

data PipelineDetails = MkPipelineDetails { pipeline   :: Pipeline
                                         , renderPass :: RenderPass
                                         }
makeRioClassy ''PipelineDetails

data Semaphores = MkSemaphores { imageAvailable :: Semaphore
                               , renderFinished :: Semaphore
                               }
makeRioClassy ''Semaphores

data VulkanApp = MkVulkanApp { app               :: App
                             , graphicsResources :: GraphicsResources
                             , pipelineDetails   :: PipelineDetails
                             , commandPool       :: CommandPool
                             , buffers           :: Vector BufferCollection
                             , semaphores        :: Semaphores
                             }
makeRioClassy ''VulkanApp

data SwapchainSupportDetails = MkSwapchainSupportDetails { swapchainCapabilities :: SurfaceCapabilitiesKHR
                                                         , swapchainFormats      :: NonEmpty SurfaceFormatKHR
                                                         , swapchainPresentModes :: NonEmpty PresentModeKHR
                                                         }
