{-# LANGUAGE OverloadedLists #-}

module VulkanConfig.Pipeline where

import RIO hiding (logInfo, logWarn, logError, logDebug)

import Vulkan hiding ( MacOSSurfaceCreateInfoMVK(view)
                     , IOSSurfaceCreateInfoMVK(view)
                     , Display
                     , WaylandSurfaceCreateInfoKHR(display)
                     , DisplayPropertiesKHR(display)
                     )
import Vulkan.Zero
import Vulkan.CStruct.Extends

import Types
import Utils
import Foreign (sizeOf)

-- This can probably be improved
numVertices, offset0, offset1, sizeVertex :: Word32
numVertices = 5
offset0 = 0
offset1 = 2
sizeVertex = 5 * fromIntegral (sizeOf (0.0 :: Float))

-- Could probably do SVector (SVector Float), would be nicer.
-- We'd have to have an existential type for ?vertexData.
vertexData :: SVector Float
vertexData = [ 0.1, 0.2,  0.3, 0.4, 0.5
             , 0.2, 0.3,  0.4, 0.5, 0.6
             , 0.3, 0.4,  0.5, 0.6, 0.7
             , 0.4, 0.5,  0.6, 0.7, 0.8
             , 0.5, 0.6,  0.7, 0.8, 0.9
             ]

setupGraphicsCommands :: (MonadIO m, HasLogger, HasVulkanResources) => m ()
setupGraphicsCommands = do
  MkMutables{imageRelateds, renderPass, swapchainExtent, graphicsPipeline} <- readRes ?mutables
  for_ imageRelateds \MkImageRelated{commandBuffer, framebuffer} -> do
    useCommandBuffer commandBuffer zero do
      let renderPassInfo = zero{ renderPass
                               , framebuffer
                               , renderArea = zero{extent = swapchainExtent}
                               , clearValues = [Color $ Float32 0 0 0 1]
                               }
      cmdUseRenderPass commandBuffer renderPassInfo SUBPASS_CONTENTS_INLINE do
        cmdBindPipeline commandBuffer PIPELINE_BIND_POINT_GRAPHICS graphicsPipeline

        cmdBindVertexBuffers commandBuffer 0 [?vertexBuffer] [0]

        cmdDraw commandBuffer numVertices 1 0 0
  logDebug "Set up commands."

graphicsPipelineLayoutInfo :: PipelineLayoutCreateInfo
graphicsPipelineLayoutInfo = zero

vertexInputInfo :: SomeStruct PipelineVertexInputStateCreateInfo
vertexInputInfo = SomeStruct zero{vertexBindingDescriptions, vertexAttributeDescriptions}
  where
    vertexBindingDescriptions = [ zero { binding = 0
                                       , stride = sizeVertex
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
vertexBufferInfo = zero { size = ((*) `on` fromIntegral) numVertices sizeVertex
                        , usage = BUFFER_USAGE_VERTEX_BUFFER_BIT
                        , sharingMode = SHARING_MODE_EXCLUSIVE
                        }
