{-# LANGUAGE OverloadedLists #-}

module Pipeline where

import RIO

import Vulkan hiding ( MacOSSurfaceCreateInfoMVK(view)
                     , IOSSurfaceCreateInfoMVK(view)
                     , Display
                     , WaylandSurfaceCreateInfoKHR(display)
                     , DisplayPropertiesKHR(display)
                     )
import Vulkan.Zero

import Types
import Shaders (vertPath, fragPath)
import Data.Bits ((.|.))
import Vulkan.CStruct.Extends (SomeStruct (SomeStruct))
import Control.Monad.Trans.Resource (MonadResource, allocate, ReleaseKey, release)

loadShader :: MonadResource m => Device -> FilePath -> m (ReleaseKey, ShaderModule)
loadShader device path = do
  bytes <- readFileBinary path
  withShaderModule device zero{code = bytes} Nothing allocate

withGraphicsPipelineLayout :: MonadResource m => Device -> m (ReleaseKey, PipelineLayout)
withGraphicsPipelineLayout device = do
  let layoutInfo = zero
  withPipelineLayout device layoutInfo Nothing allocate

withGraphicsPipeline :: MonadResource m => GraphicsResources -> m PipelineDetails
withGraphicsPipeline graphicsResources = do
  let extent@Extent2D{width, height} = graphicsResources^.swapchainExtentL
      device = graphicsResources^.deviceL
      vertexInputState = Just zero
      inputAssemblyState = Just zero{ topology = PRIMITIVE_TOPOLOGY_POINT_LIST }
      viewport = zero{ width = fromIntegral width
                     , height = fromIntegral height
                     , minDepth  =0
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
  (_, layout) <- withGraphicsPipelineLayout device
  (_, renderPass) <- withGraphicsRenderPass graphicsResources

  (releaseVert, vertModule) <- loadShader device vertPath
  (releaseFrag, fragModule) <- loadShader device fragPath

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

  (_, (_, pipelines)) <- withGraphicsPipelines device zero pipelineInfo Nothing allocate

  -- We don't need the shader modules anymore after creating the pipeline
  traverse_ @[] release [releaseVert, releaseFrag]

  case pipelines of
    [pipeline]      -> pure $ MkPipelineDetails pipeline renderPass
    (length -> num) -> throwIO $ VkWrongNumberOfGraphicsPipelines 1 $ fromIntegral num

withGraphicsRenderPass :: MonadResource m => GraphicsResources -> m (ReleaseKey, RenderPass)
withGraphicsRenderPass graphicsResources = do
  let SurfaceFormatKHR{format} = graphicsResources^.swapchainFormatL
      device = graphicsResources^.deviceL
      colorAttachment = zero{ format
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

  withRenderPass device renderPassInfo Nothing allocate
