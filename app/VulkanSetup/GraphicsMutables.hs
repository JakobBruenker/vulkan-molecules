{-# LANGUAGE OverloadedLists #-}

module VulkanSetup.GraphicsMutables where

import RIO hiding (logDebug, logInfo, logWarn, logError)
import RIO.Vector qualified as V
import RIO.NonEmpty qualified as NE

import Control.Lens (both, allOf, ifor_, Ixed (ix))
import Data.Bits ((.|.))
import Data.Foldable (find)
import Data.List (nub)
import Control.Monad.Trans.Resource (allocate, ReleaseKey, ResIO)
import Foreign.Storable.Tuple ()
import Control.Monad.Loops (whileM_)

import Graphics.UI.GLFW qualified as GLFW

import Vulkan hiding ( MacOSSurfaceCreateInfoMVK(view)
                     , IOSSurfaceCreateInfoMVK(view)
                     , Display
                     , WaylandSurfaceCreateInfoKHR(display)
                     , DisplayPropertiesKHR(display)
                     )
import Vulkan.Zero
import Vulkan.CStruct.Extends (SomeStruct (SomeStruct))

import Utils
import VulkanSetup.Types
import VulkanSetup.Utils

initGraphicsMutables :: (HasLogger, HasGraphicsResources, HasShaderPaths, HasVulkanConfig,
                         HasGraphicsUniformBuffers)
                     => ResIO (Dict HasGraphicsMutables)
initGraphicsMutables = do
  graphicsMutables <- mkMResources =<< constructGraphicsMutables
  let ?graphicsMutables = graphicsMutables
  pure Dict

constructGraphicsMutables :: (HasLogger, HasGraphicsResources, HasShaderPaths, HasVulkanConfig,
                              HasGraphicsUniformBuffers)
                          => ResIO ([ReleaseKey], GraphicsMutables)
constructGraphicsMutables = do
  scInfo <- swapchainCreateInfo
  let swapchainExtent = scInfo.imageExtent
      swapchainFormat = scInfo.imageFormat
  (swapchainKey , swapchain     ) <- constructSwapchain scInfo
  (renderPassKey, renderPass    ) <- constructGraphicsRenderPass swapchainFormat
  (layoutKey    , pipelineLayout) <- constructGraphicsPipelineLayout
  (pipelineKey  , pipeline      ) <-
    constructGraphicsPipeline renderPass swapchainExtent pipelineLayout
  (imageKeys    , imageRelateds) <-
    constructImageRelateds swapchainExtent swapchainFormat renderPass swapchain

  pure ([swapchainKey, renderPassKey, layoutKey, pipelineKey] <> imageKeys, MkGraphicsMutables{..})

constructSwapchain :: (HasLogger, HasDevice)
                   => SwapchainCreateInfoKHR '[] -> ResIO (ReleaseKey, SwapchainKHR)
constructSwapchain scInfo = do
  swapchain <- withSwapchainKHR ?device scInfo Nothing allocate
  logDebug "Created swapchain."
  pure swapchain

waitWhileMinimized :: (HasWindow, MonadIO m) => m ()
waitWhileMinimized = liftIO $ whileM_ (allOf both (== 0) <$> GLFW.getFramebufferSize ?window)
  GLFW.waitEvents

swapchainCreateInfo :: (MonadIO m, HasLogger, HasGraphicsResources, HasDesiredSwapchainImageNum)
                    => m (SwapchainCreateInfoKHR '[])
swapchainCreateInfo = do

  waitWhileMinimized

  SurfaceCapabilitiesKHR{ currentExtent, currentTransform
                        , minImageExtent, maxImageExtent
                        , minImageCount, maxImageCount
                        } <- getPhysicalDeviceSurfaceCapabilitiesKHR ?physicalDevice ?surface
  let SurfaceFormatKHR{format = imageFormat, colorSpace = imageColorSpace} = liftA2 fromMaybe NE.head
        (find (== SurfaceFormatKHR FORMAT_R8G8B8A8_SRGB COLOR_SPACE_SRGB_NONLINEAR_KHR))
        ?swapchainFormats
      queueFamilyIndices = V.fromList $ nub [?graphicsQueueFamily, ?presentQueueFamily]

  imageExtent <- do
    let Extent2D{width = currentWidth} = currentExtent
        Extent2D{width = minWidth, height = minHeight} = minImageExtent
        Extent2D{width = maxWidth, height = maxHeight} = maxImageExtent
    if | currentWidth /= maxBound -> pure currentExtent
       | otherwise -> do
           (fbWidth, fbHeight) <- over both fromIntegral <$> liftIO (GLFW.getFramebufferSize ?window)
           let width = clamp (minWidth, maxWidth) fbWidth
               height = clamp (minHeight, maxHeight) fbHeight
           pure Extent2D{width, height}

  let imageSharingMode | length queueFamilyIndices <= 1 = SHARING_MODE_EXCLUSIVE
                       | otherwise                      = SHARING_MODE_CONCURRENT
      imageCount = clamp (minImageCount, bool maxBound maxImageCount $ maxImageCount > 0) $
                         fromIntegral ?desiredSwapchainImageNum
      presentModesByPriority = [ PRESENT_MODE_MAILBOX_KHR
                               , PRESENT_MODE_IMMEDIATE_KHR
                               , PRESENT_MODE_FIFO_RELAXED_KHR
                               , PRESENT_MODE_FIFO_KHR
                               ] :: [PresentModeKHR]
      presentMode = fromMaybe (NE.head ?swapchainPresentModes) $
        find (`elem` ?swapchainPresentModes) presentModesByPriority

  logDebug $ "Using " <> displayShow presentMode <> "."

  pure zero{ surface = ?surface
           , minImageCount = imageCount
           , imageFormat
           , imageColorSpace
           , imageExtent
           , imageArrayLayers = 1
           , imageUsage = IMAGE_USAGE_COLOR_ATTACHMENT_BIT
           , imageSharingMode
           , queueFamilyIndices
           , preTransform = currentTransform
           , compositeAlpha = COMPOSITE_ALPHA_OPAQUE_BIT_KHR
           , presentMode
           , clipped = True
           }

constructGraphicsPipelineLayout :: (HasDevice, HasGraphicsDescriptorSetLayout,
                                    HasGraphicsPipelineLayoutInfo)
                                => ResIO (ReleaseKey, PipelineLayout)
constructGraphicsPipelineLayout =
  withPipelineLayout
    ?device ?graphicsPipelineLayoutInfo{setLayouts = [?graphicsDescriptorSetLayout]} Nothing allocate

constructGraphicsPipeline :: (HasLogger, HasDevice, HasShaderPaths, HasVulkanConfig)
                          => RenderPass -> Extent2D -> PipelineLayout -> ResIO (ReleaseKey, Pipeline)
constructGraphicsPipeline renderPass extent layout = do
  let vertexInputState = Just ?vertexInputInfo
      inputAssemblyState = Just zero{topology = PRIMITIVE_TOPOLOGY_POINT_LIST}
      viewport = zero{ width = fromIntegral extent.width
                     , height = fromIntegral extent.height
                     , minDepth = 0
                     , maxDepth = 1
                     }
      scissor = zero{extent} :: Rect2D
      viewportState = Just $ SomeStruct zero{ viewports = [viewport]
                                            , scissors = [scissor]
                                            }
      rasterizationState = SomeStruct zero{ polygonMode = POLYGON_MODE_FILL
                                          , lineWidth = 1
                                          , cullMode = CULL_MODE_BACK_BIT
                                          , frontFace = FRONT_FACE_CLOCKWISE
                                          }
      multisampleState = Just $ SomeStruct zero{ rasterizationSamples = SAMPLE_COUNT_1_BIT
                                               , minSampleShading = 1
                                               }
      blendAttachment = zero{ colorWriteMask = COLOR_COMPONENT_R_BIT
                                           .|. COLOR_COMPONENT_G_BIT
                                           .|. COLOR_COMPONENT_B_BIT
                                           .|. COLOR_COMPONENT_A_BIT
                            , blendEnable = True
                            , srcColorBlendFactor = BLEND_FACTOR_SRC_ALPHA
                            , dstColorBlendFactor = BLEND_FACTOR_ONE_MINUS_SRC_ALPHA
                            , colorBlendOp = BLEND_OP_ADD
                            , srcAlphaBlendFactor = BLEND_FACTOR_ONE
                            , dstAlphaBlendFactor = BLEND_FACTOR_ZERO
                            , alphaBlendOp = BLEND_OP_ADD
                            }
      colorBlendState = Just $ SomeStruct zero{ logicOpEnable = False
                                              , attachments = [blendAttachment]
                                              }

  (releasePipelines, (_, pipelines)) <-
    withShader ?vertexShaderPath \vertModule ->
      withShader ?fragmentShaderPath \fragModule -> do

        let vertShaderStageInfo = zero{ stage = SHADER_STAGE_VERTEX_BIT
                                      , module' = vertModule
                                      , name = "main"
                                      }
            fragShaderStageInfo = zero{ stage = SHADER_STAGE_FRAGMENT_BIT
                                      , module' = fragModule
                                      , name = "main"
                                      }
            shaderStages = SomeStruct <$> [vertShaderStageInfo, fragShaderStageInfo]
            pipelineInfo = pure $ SomeStruct zero{ stages = shaderStages
                                                 , vertexInputState
                                                 , inputAssemblyState
                                                 , viewportState
                                                 , rasterizationState
                                                 , multisampleState
                                                 , colorBlendState
                                                 , layout
                                                 , renderPass
                                                 , subpass = 0
                                                 }

        withGraphicsPipelines ?device zero pipelineInfo Nothing allocate

  graphicsPipeline <- case pipelines of
    [pipeline] -> pure (releasePipelines, pipeline)
    (length -> num) -> throwIO $ VkWrongNumberOfGraphicsPipelines 1 $ fromIntegral num

  logDebug "Created graphics pipeline."
  pure graphicsPipeline

constructGraphicsRenderPass :: (HasLogger, HasGraphicsResources)
                            => Format -> ResIO (ReleaseKey, RenderPass)
constructGraphicsRenderPass format = do
  let colorAttachment = zero{ format
                            , samples = SAMPLE_COUNT_1_BIT
                            , loadOp = ATTACHMENT_LOAD_OP_CLEAR
                            , storeOp = ATTACHMENT_STORE_OP_STORE
                            , stencilLoadOp = ATTACHMENT_LOAD_OP_DONT_CARE
                            , stencilStoreOp = ATTACHMENT_STORE_OP_DONT_CARE
                            , initialLayout = IMAGE_LAYOUT_UNDEFINED
                            , finalLayout = IMAGE_LAYOUT_PRESENT_SRC_KHR
                            } :: AttachmentDescription
      colorAttachmentRef = zero{ attachment = 0
                               , layout = IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
                               } :: AttachmentReference
      subpass = zero{ pipelineBindPoint = PIPELINE_BIND_POINT_GRAPHICS
                    , colorAttachments = [colorAttachmentRef]
                    } :: SubpassDescription
      dependency = zero{ srcSubpass = SUBPASS_EXTERNAL
                       , dstSubpass = 0
                       , srcStageMask = PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
                       , srcAccessMask = zero
                       , dstStageMask = PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
                       , dstAccessMask = ACCESS_COLOR_ATTACHMENT_WRITE_BIT
                       } :: SubpassDependency
      renderPassInfo = zero{ attachments = [colorAttachment]
                           , subpasses = [subpass]
                           , dependencies = [dependency]
                           } :: RenderPassCreateInfo '[]

  renderPass <- withRenderPass ?device renderPassInfo Nothing allocate

  logDebug "Created render pass."
  pure renderPass

constructImageRelateds :: (HasLogger, HasDevice, HasGraphicsCommandPool,
                           HasGraphicsDescriptorSetLayout, HasGraphicsUniformBufferSize,
                           HasGraphicsUniformBuffers)
                       => Extent2D -> Format -> RenderPass -> SwapchainKHR
                       -> ResIO ([ReleaseKey], Vector ImageRelated)
constructImageRelateds extent format renderPass swapchain = do
  (_, images) <- getSwapchainImagesKHR ?device swapchain
  let count = fromIntegral $ length images
  (cmdBufKey, commandBuffers) <- constructCommandBuffers count
  (dstKey, descriptorSets) <- constructDescriptorSets count
  (releaseKeys, imageRelateds) <- fmap V.unzip . sequence $
    V.zipWith3 (constructImageRelated extent format renderPass) images commandBuffers descriptorSets

  logDebug "Created images."
  pure (cmdBufKey : dstKey : concat releaseKeys, imageRelateds)

constructCommandBuffers :: (HasDevice, HasGraphicsCommandPool)
                        => Natural -> ResIO (ReleaseKey, Vector CommandBuffer)
constructCommandBuffers count = do
  let commandBuffersInfo = zero{ commandPool = ?graphicsCommandPool
                               , level = COMMAND_BUFFER_LEVEL_PRIMARY
                               , commandBufferCount = fromIntegral count
                               }
  withCommandBuffers ?device commandBuffersInfo allocate

constructDescriptorSets :: (HasDevice, HasGraphicsDescriptorSetLayout,
                            HasGraphicsUniformBufferSize, HasGraphicsUniformBuffers)
                        => Natural -> ResIO (ReleaseKey, Vector DescriptorSet)
constructDescriptorSets count = do
  let descriptorCount :: Integral a => a
      descriptorCount = fromIntegral count
      poolSize = zero{descriptorCount, type' = DESCRIPTOR_TYPE_UNIFORM_BUFFER}
      poolInfo = zero{poolSizes = [poolSize], maxSets = descriptorCount}
  (poolKey, descriptorPool) <- withDescriptorPool ?device poolInfo Nothing allocate

  let allocInfo =
        zero{descriptorPool, setLayouts = V.replicate descriptorCount ?graphicsDescriptorSetLayout}
  descriptorSets <- allocateDescriptorSets ?device allocInfo

  ifor_ descriptorSets \i dstSet -> do
    buffer <- maybe
      do throwIO VkUniformBufferIndexOutOfRange
      do pure . fst
      do ?graphicsUniformBuffers ^? ix i
    let bufferInfo = [ zero{ buffer
                           , offset = 0
                           , range = ?graphicsUniformBufferSize
                           } :: DescriptorBufferInfo
                     ]
        descriptorWrite = SomeStruct zero{ dstSet
                                         , dstBinding = 0
                                         , dstArrayElement = 0
                                         , descriptorType = DESCRIPTOR_TYPE_UNIFORM_BUFFER
                                         , descriptorCount = 1
                                         , bufferInfo
                                         } :: SomeStruct WriteDescriptorSet
    updateDescriptorSets ?device [descriptorWrite] []
  pure (poolKey, descriptorSets)

constructImageRelated :: HasDevice
                      => Extent2D -> Format -> RenderPass -> Image -> CommandBuffer -> DescriptorSet
                      -> ResIO ([ReleaseKey], ImageRelated)
constructImageRelated extent format renderPass image commandBuffer descriptorSet = do
  imageInFlight                 <- constructImageInFlight
  (imgViewKey , imageView     ) <- constructImageView image format
  (framebufKey, framebuffer   ) <- constructFramebuffer extent renderPass imageView
  pure ([imgViewKey, framebufKey], MkImageRelated{..})

constructImageInFlight :: MonadIO m => m (IORef (Maybe Fence))
constructImageInFlight = newIORef Nothing

constructImageView :: HasDevice => Image -> Format -> ResIO (ReleaseKey, ImageView)
constructImageView image format = do
  let ivInfo = zero{ viewType = IMAGE_VIEW_TYPE_2D
                   , format
                   , subresourceRange
                   }
      subresourceRange = zero{ aspectMask = IMAGE_ASPECT_COLOR_BIT
                             , levelCount = 1
                             , layerCount = 1
                             }

  withImageView ?device ivInfo{image} Nothing allocate

constructFramebuffer :: HasDevice
                     => Extent2D -> RenderPass -> ImageView -> ResIO (ReleaseKey, Framebuffer)
constructFramebuffer Extent2D{width, height} renderPass imageView = do
  let fbInfo = zero{ renderPass
                   , width
                   , height
                   , layers = 1
                   }
  withFramebuffer ?device fbInfo{attachments = [imageView]} Nothing allocate

recreateSwapchain :: (HasLogger, HasVulkanResources)
                  => (HasVulkanResources => ResIO ()) -> ResIO ()
recreateSwapchain setupCommands = do
  logDebug "Recreating swapchain..."

  writeIORef ?framebufferResized False

  deviceWaitIdle ?device

  writeRes ?graphicsMutables constructGraphicsMutables

  setupCommands
