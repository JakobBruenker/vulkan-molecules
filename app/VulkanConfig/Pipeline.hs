{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE MagicHash #-}

module VulkanConfig.Pipeline where

import RIO hiding (logInfo, logWarn, logError, logDebug)
import Data.Vector.Storable.Sized qualified as Sized

import Foreign.Storable.Tuple ()
import Foreign (sizeOf, (.|.))
import GHC.Exts (proxy#)

import Vulkan hiding ( MacOSSurfaceCreateInfoMVK(view)
                     , IOSSurfaceCreateInfoMVK(view)
                     , Display
                     , WaylandSurfaceCreateInfoKHR(display)
                     , DisplayPropertiesKHR(display)
                     )
import Vulkan.Zero
import Vulkan.CStruct.Extends

import VulkanSetup.Types
import Utils

type SizeFloat = 4

type NumVertices = 19
type Size0 = 2
type Size1 = 3

numVertices, numVertexEntries, floatSize, vertexSize, offset0, offset1 :: Word32
numVertices = fromIntegral $ Sized.length VulkanConfig.Pipeline.vertexData
numVertexEntries = integralNatVal @Size0 + integralNatVal @Size1
floatSize = fromIntegral $ sizeOf (0 :: Float)
vertexSize = floatSize * numVertexEntries
offset0 = 0
offset1 = floatSize * (numVertexEntries - integralNatVal @Size1)

-- 2D position, RGB color
vertexData :: Sized.Vector NumVertices (Sized.Vector Size0 Float, Sized.Vector Size1 Float)
vertexData = Sized.fromTuple
  ( vertex ( 0.5, -0.9) ( 0.5,  0.7,  0.9)
  , vertex ( 0.1, -0.8) ( 0.5,  0.7,  0.9)
  , vertex (-0.2, -0.7) ( 0.5,  0.7,  0.9)
  , vertex (-0.3, -0.6) ( 0.5,  0.7,  0.9)
  , vertex (-0.7, -0.5) ( 0.5,  0.7,  0.9)
  , vertex ( 0.6, -0.4) ( 0.5,  0.7,  0.9)
  , vertex (-0.2, -0.3) ( 0.5,  0.7,  0.9)
  , vertex ( 0.8, -0.2) ( 0.5,  0.7,  0.9)
  , vertex ( 0.1, -0.1) ( 0.5,  0.7,  0.9)
  , vertex (-0.9,  0  ) ( 0.5,  0.7,  0.9)
  , vertex (-0.5,  0.1) ( 0.5,  0.7,  0.9)
  , vertex ( 0.6,  0.2) ( 0.5,  0.7,  0.9)
  , vertex ( 0.7,  0.3) ( 0.5,  0.7,  0.9)
  , vertex (-0.1,  0.4) ( 0.5,  0.7,  0.9)
  , vertex ( 0.2,  0.5) ( 0.5,  0.7,  0.9)
  , vertex (-0.2,  0.6) ( 0.5,  0.7,  0.9)
  , vertex (-0.9,  0.7) ( 0.5,  0.7,  0.9)
  , vertex (-0.3,  0.8) ( 0.5,  0.7,  0.9)
  , vertex ( 0.4,  0.9) ( 0.5,  0.7,  0.9)
  )
  where vertex (a, b) (c, d, e) = (Sized.fromTuple (a, b), Sized.fromTuple (c, d, e))

type GraphicsUboContents = ("time" ::: Float, "window width" ::: Int32, "window height" ::: Int32)
type instance UboInput Graphics = ("window width" ::: Int32, "window height" ::: Int32)

graphicsUboData :: MonadIO m => m (UboData Graphics)
graphicsUboData = MkUboData proxy# (\(w, h) (i, _, _) -> (i + 1, w, h)) <$>
  newIORef @_ @GraphicsUboContents (0, 0, 0)

type ComputeUboContents = ("index" ::: Word32)
type instance UboInput Compute = ()

computeUboData :: MonadIO m => m (UboData Compute)
computeUboData = MkUboData proxy# (\_ i -> (i + 1) `mod` 2) <$>
  newIORef @_ @ComputeUboContents 1

setupComputeCommands :: (MonadIO m, HasLogger, HasVulkanResources) => m ()
setupComputeCommands = do
  mutables <- readRes ?computeMutables
  useCommandBuffer mutables.commandBuffer zero{flags = COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT} do
    -- TODO: consider having VERTEX_SHADER_BIT as dstStageMask specifically for
    -- updPos if it doesn't negatively impact performance
    let memoryBarrier = zero{ srcAccessMask = ACCESS_SHADER_WRITE_BIT
                            , dstAccessMask = ACCESS_SHADER_READ_BIT
                            } :: MemoryBarrier
        stageMask = PIPELINE_STAGE_TRANSFER_BIT .|. PIPELINE_STAGE_COMPUTE_SHADER_BIT
    for_ (Sized.toList mutables.pipelines) \pipeline -> do
      cmdPipelineBarrier mutables.commandBuffer stageMask stageMask zero [memoryBarrier] [] []
      cmdBindPipeline mutables.commandBuffer PIPELINE_BIND_POINT_COMPUTE pipeline
      cmdBindDescriptorSets mutables.commandBuffer PIPELINE_BIND_POINT_COMPUTE
                            mutables.pipelineLayout 0 mutables.descriptorSets []
      cmdDispatch mutables.commandBuffer 1 1 1

  logDebug "Set up compute commands"

setupGraphicsCommands :: (MonadIO m, HasLogger, HasVulkanResources) => m ()
setupGraphicsCommands = do
  mutables <- readRes ?graphicsMutables
  for_ mutables.imageRelateds \ir -> do
    useCommandBuffer ir.commandBuffer zero do
      let renderPassInfo = zero{ renderPass = mutables.renderPass
                               , framebuffer = ir.framebuffer
                               , renderArea = zero{extent = mutables.swapchainExtent}
                               , clearValues = [Color $ Float32 0 0 0 1]
                               }
      cmdUseRenderPass ir.commandBuffer renderPassInfo SUBPASS_CONTENTS_INLINE do
        cmdBindPipeline ir.commandBuffer PIPELINE_BIND_POINT_GRAPHICS mutables.pipeline

        cmdBindVertexBuffers ir.commandBuffer 0 [?vertexBuffer] [0]

        cmdBindDescriptorSets ir.commandBuffer PIPELINE_BIND_POINT_GRAPHICS
                              mutables.pipelineLayout 0 [ir.descriptorSet] []
        cmdDraw ir.commandBuffer numVertices 1 0 0
  logDebug "Set up graphics commands."

graphicsPipelineLayoutInfo :: PipelineLayoutCreateInfo
graphicsPipelineLayoutInfo = zero

computePipelineLayoutInfo :: PipelineLayoutCreateInfo
computePipelineLayoutInfo = zero

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
                        , usage = BUFFER_USAGE_VERTEX_BUFFER_BIT
                              .|. BUFFER_USAGE_TRANSFER_DST_BIT
                              .|. BUFFER_USAGE_STORAGE_BUFFER_BIT
                        , sharingMode = SHARING_MODE_EXCLUSIVE
                        }

graphicsDescriptorSetLayoutInfo :: DescriptorSetLayoutCreateInfo '[]
graphicsDescriptorSetLayoutInfo = zero{bindings = graphicsDescriptorSetBindings}
  where
    graphicsDescriptorSetBindings = [ zero{ binding = 0
                                          , descriptorType = DESCRIPTOR_TYPE_UNIFORM_BUFFER
                                          , descriptorCount = 1
                                          , stageFlags = SHADER_STAGE_VERTEX_BIT
                                          }
                                    ]

computeDescriptorSetLayoutInfo :: Vector (DescriptorSetLayoutCreateInfo '[])
computeDescriptorSetLayoutInfo = [ zero{bindings = uniformBindings}
                                 , zero{bindings = storageBindings}
                                 ]
  where
    uniformBindings = [ zero{ binding = 0
                            , descriptorType = DESCRIPTOR_TYPE_UNIFORM_BUFFER
                            , descriptorCount = 1
                            , stageFlags = SHADER_STAGE_COMPUTE_BIT
                            }
                      ]
    storageBindings = [ zero{ binding = 0
                            , descriptorType = DESCRIPTOR_TYPE_STORAGE_BUFFER
                            , descriptorCount = 1
                            , stageFlags = SHADER_STAGE_COMPUTE_BIT
                            }
                      ]

vulkanConfig :: MonadIO m => m (Dict HasVulkanConfig)
vulkanConfig = do
  graphicsUboData'@(MkUboData graphicsUboProxy _ _) <- graphicsUboData
  computeUboData'@(MkUboData computeUboProxy _ _) <- computeUboData
  let ?graphicsPipelineLayoutInfo = graphicsPipelineLayoutInfo
      ?computePipelineLayoutInfo = computePipelineLayoutInfo
      ?vertexInputInfo = vertexInputInfo
      ?vertexBufferInfo = vertexBufferInfo
      ?vertexData = MkVertexData VulkanConfig.Pipeline.vertexData
      ?graphicsDescriptorSetLayoutInfo = graphicsDescriptorSetLayoutInfo
      ?computeDescriptorSetLayoutInfo = computeDescriptorSetLayoutInfo
      ?graphicsUniformBufferSize = fromIntegral $ sizeOfProxied graphicsUboProxy
      ?graphicsUboData = graphicsUboData'
      ?computeUniformBufferSize = fromIntegral $ sizeOfProxied computeUboProxy
      ?computeUboData = computeUboData'
      ?desiredSwapchainImageNum = 3
  pure Dict
