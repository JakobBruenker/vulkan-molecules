{-# LANGUAGE OverloadedLists #-}

module VulkanConfig.Pipeline where

import RIO hiding (logInfo, logWarn, logError, logDebug)
import Data.Vector.Storable.Sized qualified as Sized

import Foreign.Storable.Tuple ()

import Vulkan hiding ( MacOSSurfaceCreateInfoMVK(view)
                     , IOSSurfaceCreateInfoMVK(view)
                     , Display
                     , WaylandSurfaceCreateInfoKHR(display)
                     , DisplayPropertiesKHR(display)
                     )
import Vulkan.Zero
import Vulkan.CStruct.Extends

import Graphics.Types
import Utils
import Foreign (sizeOf, (.|.))

type UniformBufferSize = 1

type NumVertexEntries = 5
type NumVertices = 6
type Size0 = 2
type Size1 = 3

numVertices, floatSize, vertexSize, offset0, offset1 :: Word32
numVertices = integralNatVal @NumVertices
floatSize = fromIntegral $ sizeOf (0 :: Float)
vertexSize = floatSize * integralNatVal @NumVertexEntries
offset0 = 0
offset1 = floatSize * (integralNatVal @NumVertexEntries - integralNatVal @Size1)

-- 2D position, RGB color
vertexData :: Sized.Vector NumVertices (Sized.Vector Size0 Float, Sized.Vector Size1 Float)
vertexData = Sized.fromTuple
  ( vertex ( 0.1,  0.2) ( 0.3,  0.4,  0.5)
  , vertex ( 0.2,  0.3) ( 0.4,  0.5,  0.6)
  , vertex ( 0.3,  0.4) ( 0.5,  0.6,  0.7)
  , vertex ( 0.4,  0.5) ( 0.6,  0.7,  0.8)
  , vertex ( 0.5,  0.6) ( 0.7,  0.8,  0.9)
  , vertex (-0.2,  0.6) ( 1  ,  0.1,  0  )
  )
  where vertex (a, b) (c, d, e) = (Sized.fromTuple (a, b), Sized.fromTuple (c, d, e))

setupGraphicsCommands :: (MonadIO m, HasLogger, HasVulkanResources) => m ()
setupGraphicsCommands = do
  MkMutables{imageRelateds, renderPass, swapchainExtent, graphicsPipelineLayout, graphicsPipeline} <-
    readRes ?mutables
  for_ imageRelateds \MkImageRelated{framebuffer, commandBuffer, descriptorSet} -> do
    useCommandBuffer commandBuffer zero do
      let renderPassInfo = zero{ renderPass
                               , framebuffer
                               , renderArea = zero{extent = swapchainExtent}
                               , clearValues = [Color $ Float32 0 0 0 1]
                               }
      cmdUseRenderPass commandBuffer renderPassInfo SUBPASS_CONTENTS_INLINE do
        cmdBindPipeline commandBuffer PIPELINE_BIND_POINT_GRAPHICS graphicsPipeline

        cmdBindVertexBuffers commandBuffer 0 [?vertexBuffer] [0]

        cmdBindDescriptorSets commandBuffer PIPELINE_BIND_POINT_GRAPHICS graphicsPipelineLayout
                              0 [descriptorSet] []
        cmdDraw commandBuffer numVertices 1 0 0
  logDebug "Set up commands."

graphicsPipelineLayoutInfo :: PipelineLayoutCreateInfo
graphicsPipelineLayoutInfo = zero

vertexInputInfo :: SomeStruct PipelineVertexInputStateCreateInfo
vertexInputInfo = SomeStruct zero{vertexBindingDescriptions, vertexAttributeDescriptions}
  where
    vertexBindingDescriptions = [ zero { binding = 0
                                       , stride = vertexSize
                                       , inputRate = VERTEX_INPUT_RATE_VERTEX
                                       } :: VertexInputBindingDescription
                                ]
    vertexAttributeDescriptions = [ zero { binding = 0
                                         , location = 0
                                         , format = FORMAT_R32G32_SFLOAT
                                         , offset = offset0
                                         } :: VertexInputAttributeDescription
                                  , zero { binding = 0
                                         , location = 1
                                         , format = FORMAT_R32G32B32_SFLOAT
                                         , offset = offset1
                                         } :: VertexInputAttributeDescription
                                  ]

vertexBufferInfo :: BufferCreateInfo '[]
vertexBufferInfo = zero { size = ((*) `on` fromIntegral) numVertices vertexSize
                        , usage = BUFFER_USAGE_VERTEX_BUFFER_BIT .|. BUFFER_USAGE_TRANSFER_DST_BIT
                        , sharingMode = SHARING_MODE_EXCLUSIVE
                        }

descriptorSetLayoutInfo :: DescriptorSetLayoutCreateInfo '[]
descriptorSetLayoutInfo = zero{bindings = descriptorSetBindings}
  where
    descriptorSetBindings = [ zero{ binding = 0
                                  , descriptorType = DESCRIPTOR_TYPE_UNIFORM_BUFFER
                                  , descriptorCount = 1
                                  , stageFlags = SHADER_STAGE_VERTEX_BIT
                                  }
                            ]

uboData :: MonadIO m => m UboData
uboData = MkUboData (+ 1) <$> newIORef @_ @Float 0

vulkanConfig :: MonadIO m => m (Dict HasVulkanConfig)
vulkanConfig = do
  uboData' <- uboData
  let ?graphicsPipelineLayoutInfo = graphicsPipelineLayoutInfo
      ?vertexInputInfo = vertexInputInfo
      ?vertexBufferInfo = vertexBufferInfo
      ?vertexData = MkVertexData VulkanConfig.Pipeline.vertexData
      ?descriptorSetLayoutInfo = descriptorSetLayoutInfo
      ?uniformBufferSize = integralNatVal @UniformBufferSize
      ?desiredSwapchainImageNum = 3
      ?uboData = uboData'
  pure Dict
