{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE UndecidableInstances #-}

module VulkanSetup.Types.Unsafe where

import RIO

import Data.Vector.Storable.Sized qualified as Sized
import Data.Vector.Sized qualified as Sized'
import Graphics.UI.GLFW qualified as GLFW
import Control.Monad.Trans.Resource (ReleaseKey, release)

import Vulkan hiding ( Display
                     , Win32KeyedMutexAcquireReleaseInfoNV(releaseKeys)
                     , Win32KeyedMutexAcquireReleaseInfoKHR(releaseKeys)
                     )

import Vulkan.CStruct.Extends
import GHC.TypeNats (KnownNat)

import Data.Kind (Type)

type MaxFramesInFlight = 2

data Dict c = c => Dict

-- abstract type to keep track of whether GLFW is initialized
data GLFWToken = UnsafeMkGLFWToken

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
                                   , descriptorSet :: DescriptorSet
                                   }

data GraphicsMutables = MkGraphicsMutables { imageRelateds   :: Vector ImageRelated
                                           , swapchainFormat :: Format
                                           , swapchainExtent :: Extent2D
                                           , swapchain       :: SwapchainKHR
                                           , renderPass      :: RenderPass
                                           , pipelineLayout  :: PipelineLayout
                                           , pipeline        :: Pipeline
                                           , colorImage      :: Image
                                           , colorImageView  :: ImageView
                                           , depthImage      :: Image
                                           , depthImageView  :: ImageView
                                           }

data ComputeMutables = MkComputeMutables { pipelineLayout :: PipelineLayout
                                         , pipelines      :: Sized.Vector ComputeShaderCount Pipeline
                                         , descriptorSets :: Vector DescriptorSet
                                         , commandBuffer  :: CommandBuffer
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
type HasDebug                  = ?enableDebug            :: Bool
type HasConfig = ( HasFullscreen
                 , HasMonitorIndex
                 , HasEnableValidationLayers
                 , HasDebug
                 , HasWindowSize
                 )

type HasGraphicsQueue = ?graphicsQueue :: Queue
type HasPresentQueue  = ?presentQueue  :: Queue
type HasComputeQueue  = ?computeQueue  :: Queue
type HasQueues = ( HasGraphicsQueue
                 , HasPresentQueue
                 , HasComputeQueue
                 )

type HasGraphicsQueueFamily = ?graphicsQueueFamily :: Word32
type HasPresentQueueFamily  = ?presentQueueFamily  :: Word32
type HasComputeQueueFamily  = ?computeQueueFamily  :: Word32
type HasQueueFamilyIndices = ( HasGraphicsQueueFamily
                             , HasPresentQueueFamily
                             , HasComputeQueueFamily
                             )

type HasSwapchainFormats      = ?swapchainFormats      :: NonEmpty SurfaceFormatKHR
type HasSwapchainPresentModes = ?swapchainPresentModes :: NonEmpty PresentModeKHR
type HasSwapchainSupport = ( HasSwapchainFormats
                           , HasSwapchainPresentModes
                           )

type HasPhysicalDevice = ?physicalDevice :: PhysicalDevice
type HasMsaaSamples    = ?msaaSamples    :: SampleCountFlags
type HasPhysicalDeviceRelated = ( HasPhysicalDevice
                                , HasMsaaSamples
                                , HasQueueFamilyIndices
                                , HasSwapchainSupport
                                )

type HasGLFW                        = ?glfw                        :: GLFWToken
type HasWindow                      = ?window                      :: GLFW.Window
type HasFramebufferResized          = ?framebufferResized          :: IORef Bool
type HasInstance                    = ?instance                    :: Instance
type HasValidationLayers            = ?validationLayers            :: Vector ByteString
type HasDevice                      = ?device                      :: Device
type HasSurface                     = ?surface                     :: SurfaceKHR
type HasGraphicsCommandPool         = ?graphicsCommandPool         :: CommandPool
type HasGraphicsDescriptorSetLayout = ?graphicsDescriptorSetLayout :: DescriptorSetLayout
type HasGraphicsResources = ( HasGLFW
                            , HasWindow
                            , HasFramebufferResized
                            , HasInstance
                            , HasValidationLayers
                            , HasDevice
                            , HasSurface
                            , HasGraphicsCommandPool
                            , HasQueues
                            , HasPhysicalDeviceRelated
                            , HasGraphicsDescriptorSetLayout
                            )

type HasComputeCommandPool          = ?computeCommandPool          :: CommandPool
type HasComputeDescriptorSetLayouts = ?computeDescriptorSetLayouts :: Vector DescriptorSetLayout
type HasComputeFences               = ?computeFences               :: (Fence, Fence)
type HasComputeResources = ( HasComputeCommandPool
                           , HasComputeDescriptorSetLayouts
                           , HasComputeFences
                           , HasInstance
                           , HasDevice
                           )

type SyncVector = Sized.Vector MaxFramesInFlight

type HasImageAvailable = ?imageAvailable :: SyncVector Semaphore
type HasRenderFinished = ?renderFinished :: SyncVector Semaphore
type HasInFlight       = ?inFlight       :: SyncVector Fence
type HasSyncs = ( HasImageAvailable
                , HasRenderFinished
                , HasInFlight
                )

type HasGraphicsMutables = ?graphicsMutables :: MResources GraphicsMutables
type HasComputeMutables  = ?computeMutables  :: MResources ComputeMutables

type family ComputeShaderCount :: Natural
type family ComputeStorageBufferCount :: Natural

type HasVertexShaderPath   = ?vertexShaderPath   :: FilePath
type HasFragmentShaderPath = ?fragmentShaderPath :: FilePath
type HasComputeShaderPaths = ?computeShaderPaths :: Sized'.Vector ComputeShaderCount FilePath
type HasShaderPaths = ( HasVertexShaderPath
                      , HasFragmentShaderPath
                      , HasComputeShaderPaths
                      )

type UboInput :: UboUsage -> Type
type family UboInput usage
data UboUsage = Graphics | Compute

data VertexData = forall a n . (KnownNat n, Storable a) => MkVertexData (Sized.Vector n a)
data UboData usage = forall a . Storable a => MkUboData { update :: UboInput usage -> a -> a
                                                        , ref    :: IORef a
                                                        }
data StorageData = forall a n . (KnownNat n, Storable a) => MkStorageData (Sized.Vector n a)

type HasGraphicsPipelineLayoutInfo = ?graphicsPipelineLayoutInfo :: PipelineLayoutCreateInfo
type HasComputePipelineLayoutInfo  = ?computePipelineLayoutInfo  :: PipelineLayoutCreateInfo
type HasGraphicsDescriptorSetLayoutInfo =
  ?graphicsDescriptorSetLayoutInfo :: DescriptorSetLayoutCreateInfo '[]
type HasComputeDescriptorSetLayoutInfo =
  ?computeDescriptorSetLayoutInfo :: Vector (DescriptorSetLayoutCreateInfo '[])
type HasVertexInputInfo = ?vertexInputInfo :: SomeStruct PipelineVertexInputStateCreateInfo
type HasVertexBufferInfo = ?vertexBufferInfo :: BufferCreateInfo '[]
type HasVertexData = ?vertexData :: VertexData
type HasGraphicsUboData = ?graphicsUboData :: UboData Graphics
type HasComputeUboData = ?computeUboData :: UboData Compute
type HasComputeStorageData = ?computeStorageData :: Sized'.Vector ComputeStorageBufferCount StorageData
type HasDesiredSwapchainImageNum = ?desiredSwapchainImageNum :: Natural
type HasVulkanConfig = ( HasGraphicsPipelineLayoutInfo
                       , HasComputePipelineLayoutInfo
                       , HasGraphicsDescriptorSetLayoutInfo
                       , HasComputeDescriptorSetLayoutInfo
                       , HasVertexInputInfo
                       , HasVertexBufferInfo
                       , HasVertexData
                       , HasGraphicsUboData
                       , HasComputeUboData
                       , HasComputeStorageData
                       , HasDesiredSwapchainImageNum
                       )

type HasVertexBuffer           = ?vertexBuffer           :: Buffer
type HasVertexStorageBuffer    = ?vertexStorageBuffer    :: Buffer
type HasGraphicsUniformBuffers = ?graphicsUniformBuffers :: SVector (Buffer, DeviceMemory)
type HasComputeUniformBuffer   = ?computeUniformBuffer   :: (Buffer, DeviceMemory)
type HasComputeStorageBuffers  =
  ?computeStorageBuffers :: Sized'.Vector ComputeStorageBufferCount Buffer
type HasVulkanResources = ( HasVertexBuffer
                          , HasGraphicsUniformBuffers
                          , HasComputeUniformBuffer
                          , HasComputeStorageBuffers
                          , HasGraphicsResources
                          , HasGraphicsMutables
                          , HasComputeResources
                          , HasComputeMutables
                          , HasShaderPaths
                          , HasVulkanConfig
                          , HasSyncs
                          )

type HasApp = ( HasConfig
              , HasLogger
              , HasVulkanResources
              )
