{-# LANGUAGE OverloadedLists #-}

module Swapchain where

import RIO hiding (logDebug, logInfo, logWarn, logError)
import RIO.Vector qualified as V
import RIO.NonEmpty qualified as NE

import Control.Lens (both)
import Data.Bits ((.|.))
import Data.Foldable (find)
import Data.List (nub)
import Control.Monad.Trans.Resource (allocate, ReleaseKey, release, ResIO)

import Graphics.UI.GLFW qualified as GLFW

import Vulkan hiding ( MacOSSurfaceCreateInfoMVK(view)
                     , IOSSurfaceCreateInfoMVK(view)
                     , Display
                     , WaylandSurfaceCreateInfoKHR(display)
                     , DisplayPropertiesKHR(display)
                     )
import Vulkan.Zero
import Vulkan.CStruct.Extends (SomeStruct (SomeStruct))

import Types
import Utils
import Shaders (vertPath, fragPath)

initSwapchainRelated :: (HasLogger, HasGraphicsResources) => ResIO (Dict HasSwapchainRelated)
initSwapchainRelated = do
  scInfo@(SwapchainCreateInfoKHR{imageExtent, imageFormat, imageColorSpace}) <- swapchainCreateInfo
  swapchain <- mkMResource =<< withSwapchainKHR ?device scInfo Nothing allocate
  let ?swapchain = swapchain
  logDebug "Created swapchain."

  let ?swapchainFormat = SurfaceFormatKHR{format = imageFormat, colorSpace = imageColorSpace}
  swapchainExtent <- newIORef imageExtent
  let ?swapchainExtent = swapchainExtent

  Dict <- initGraphicsRenderPass
  Dict <- initGraphicsPipelineLayout
  Dict <- initGraphicsPipeline

  pure Dict

swapchainCreateInfo :: (HasLogger, HasGraphicsResources) => ResIO (SwapchainCreateInfoKHR '[])
swapchainCreateInfo = do
  let SurfaceCapabilitiesKHR{ currentExtent, currentTransform
                            , minImageExtent, maxImageExtent
                            , minImageCount, maxImageCount
                            } = ?swapchainCapabilities
      SurfaceFormatKHR{format = imageFormat, colorSpace = imageColorSpace} = liftA2 fromMaybe NE.head
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
      imageCount = clamp (minImageCount, bool maxBound maxImageCount $ maxImageCount > 0) desired
        where desired = minImageCount + 1
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

loadShader :: HasDevice => FilePath -> ResIO (ReleaseKey, ShaderModule)
loadShader path = do
  bytes <- readFileBinary path
  withShaderModule ?device zero{code = bytes} Nothing allocate

initGraphicsPipelineLayout :: HasDevice => ResIO (Dict HasGraphicsPipelineLayout)
initGraphicsPipelineLayout = do
  let layoutInfo = zero
  layout <- mkMResource =<< withPipelineLayout ?device layoutInfo Nothing allocate
  let ?graphicsPipelineLayout = layout
  pure Dict

initGraphicsPipeline :: ( HasLogger, HasGraphicsResources, HasRenderPass
                        , HasSwapchainExtent, HasGraphicsPipelineLayout)
                     => ResIO (Dict HasGraphicsPipeline)
initGraphicsPipeline = do
  extent@Extent2D{width, height} <- readIORef ?swapchainExtent
  let vertexInputState = Just zero
      inputAssemblyState = Just zero{topology = PRIMITIVE_TOPOLOGY_POINT_LIST}
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

  (releaseVert, vertModule) <- loadShader vertPath
  (releaseFrag, fragModule) <- loadShader fragPath

  renderPass <- readRes ?renderPass
  layout <- readRes ?graphicsPipelineLayout
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

  (releasePipelines, (_, pipelines)) <-
    withGraphicsPipelines ?device zero pipelineInfo Nothing allocate

  -- We don't need the shader modules anymore after creating the pipeline
  traverse_ @[] release [releaseVert, releaseFrag]

  graphicsPipeline <- case pipelines of
    [pipeline] -> mkMResource (releasePipelines, pipeline)
    (length -> num) -> throwIO $ VkWrongNumberOfGraphicsPipelines 1 $ fromIntegral num
  let ?graphicsPipeline = graphicsPipeline

  logDebug "Created pipeline."
  pure Dict

initGraphicsRenderPass :: (HasLogger, HasGraphicsResources, HasSwapchainFormat)
                       => ResIO (Dict HasRenderPass)
initGraphicsRenderPass = do
  let colorAttachment = zero{ format = ?swapchainFormat^.formatL
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

  renderPass <- mkMResource =<< withRenderPass ?device renderPassInfo Nothing allocate
  let ?renderPass = renderPass

  logDebug "Created render pass."
  pure Dict
