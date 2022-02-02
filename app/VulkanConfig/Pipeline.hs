{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE MagicHash #-}

module VulkanConfig.Pipeline where

import RIO hiding (logInfo, logWarn, logError, logDebug)
import Data.Vector.Storable.Sized qualified as Sized
import Data.Vector.Sized qualified as Sized'
import RIO.Partial qualified as Partial

import Foreign.Storable.Tuple ()
import Foreign (sizeOf, (.|.))
import GHC.Exts (proxy#)
import GHC.TypeNats (type (*))

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

-- in Angstrom
worldWidth, worldHeight :: Float
worldWidth = 1920 / 4
worldHeight = 1080 / 4

type SizeFloat = 4

type instance ComputeStorageBufferCount = 3

computeStorageData :: Sized'.Vector ComputeStorageBufferCount StorageData
computeStorageData = Sized'.replicate . MkStorageData $
  Sized.replicate @(4 * SizeFloat * NumVertices) @Float 0

type NumVertices = 144
type Size0 = 3
type Size1 = 1

numVertices, numVertexEntries, floatSize, vertexSize, offset0, offset1 :: Word32
numVertices = fromIntegral $ Sized.length VulkanConfig.Pipeline.vertexData
numVertexEntries = integralNatVal @Size0 + integralNatVal @Size1
floatSize = fromIntegral $ sizeOf (0 :: Float)
vertexSize = floatSize * numVertexEntries
offset0 = 0
offset1 = floatSize * (numVertexEntries - integralNatVal @Size1)

-- 2D position, RGB color
vertexData :: Sized.Vector NumVertices (Sized.Vector Size0 Float, Float)
vertexData = Partial.fromJust $ Sized.fromList
  [ vertex (220,   5) 0
  , vertex (220,  25) 0
  , vertex (220,  45) 0
  , vertex (220,  65) 0
  , vertex (220,  85) 0
  , vertex (220, 105) 0
  , vertex (240,   5) 0
  , vertex (240,  25) 0
  , vertex (240,  45) 0
  , vertex (240,  65) 0
  , vertex (240,  85) 0
  , vertex (240, 105) 0
  , vertex (260,   5) 0
  , vertex (260,  25) 0
  , vertex (260,  45) 0
  , vertex (260,  65) 0
  , vertex (260,  85) 0
  , vertex (260, 105) 0
  , vertex (280,   5) 0
  , vertex (280,  25) 0
  , vertex (280,  45) 0
  , vertex (280,  65) 0
  , vertex (280,  85) 0
  , vertex (280, 105) 0
  , vertex (300,   5) 0
  , vertex (300,  25) 0
  , vertex (300,  45) 0
  , vertex (300,  65) 0
  , vertex (300,  85) 0
  , vertex (300, 105) 0
  , vertex (320,   5) 0
  , vertex (320,  25) 0
  , vertex (320,  45) 0
  , vertex (320,  65) 0
  , vertex (320,  85) 0
  , vertex (320, 105) 0
  , vertex (420,   5) 0
  , vertex (420,  25) 0
  , vertex (420,  45) 0
  , vertex (420,  65) 0
  , vertex (420,  85) 0
  , vertex (420, 105) 0
  , vertex (440,   5) 0
  , vertex (440,  25) 0
  , vertex (440,  45) 0
  , vertex (440,  65) 0
  , vertex (440,  85) 0
  , vertex (440, 105) 0
  , vertex (460,   5) 0
  , vertex (460,  25) 0
  , vertex (460,  45) 0
  , vertex (460,  65) 0
  , vertex (460,  85) 0
  , vertex (460, 105) 0
  , vertex (480,   5) 0
  , vertex (480,  25) 0
  , vertex (480,  45) 0
  , vertex (480,  65) 0
  , vertex (480,  85) 0
  , vertex (480, 105) 0
  , vertex (100, 115) 0
  , vertex (100, 125) 0
  , vertex (100, 145) 0
  , vertex (100, 165) 0
  , vertex (100, 185) 0
  , vertex (100, 205) 0
  , vertex (110, 115) 0
  , vertex (110, 125) 0
  , vertex (110, 145) 0
  , vertex (110, 165) 0
  , vertex (110, 185) 0
  , vertex (110, 205) 0
  , vertex (120, 115) 0
  , vertex (120, 125) 0
  , vertex (120, 145) 0
  , vertex (120, 165) 0
  , vertex (120, 185) 0
  , vertex (120, 205) 0
  , vertex (140, 115) 0
  , vertex (140, 125) 0
  , vertex (140, 145) 0
  , vertex (140, 165) 0
  , vertex (140, 185) 0
  , vertex (140, 205) 0
  , vertex (160, 115) 0
  , vertex (160, 125) 0
  , vertex (160, 145) 0
  , vertex (160, 165) 0
  , vertex (160, 185) 0
  , vertex (160, 205) 0
  , vertex (180, 115) 0
  , vertex (180, 125) 0
  , vertex (180, 145) 0
  , vertex (180, 165) 0
  , vertex (180, 185) 0
  , vertex (180, 205) 0
  , vertex (200, 115) 0
  , vertex (200, 125) 0
  , vertex (200, 145) 0
  , vertex (200, 165) 0
  , vertex (200, 185) 0
  , vertex (200, 205) 0
  , vertex (220, 115) 0
  , vertex (220, 125) 0
  , vertex (220, 145) 0
  , vertex (220, 165) 0
  , vertex (220, 185) 0
  , vertex (220, 205) 0
  , vertex (320, 115) 0
  , vertex (320, 125) 0
  , vertex (320, 145) 0
  , vertex (320, 165) 0
  , vertex (320, 185) 0
  , vertex (320, 205) 0
  , vertex (340, 115) 0
  , vertex (340, 125) 0
  , vertex (340, 145) 0
  , vertex (340, 165) 0
  , vertex (340, 185) 0
  , vertex (340, 205) 0
  , vertex (360, 115) 0
  , vertex (360, 125) 0
  , vertex (360, 145) 0
  , vertex (360, 165) 0
  , vertex (360, 185) 0
  , vertex (360, 205) 0
  , vertex (380, 115) 0
  , vertex (380, 125) 0
  , vertex (380, 145) 0
  , vertex (380, 165) 0
  , vertex (380, 185) 0
  , vertex (380, 205) 0
  , vertex (450, 115) 0
  , vertex (450, 125) 0
  , vertex (450, 145) 0
  , vertex (450, 165) 0
  , vertex (450, 185) 0
  , vertex (450, 205) 0
  , vertex (470, 115) 0
  , vertex (470, 125) 0
  , vertex (470, 145) 0
  , vertex (470, 165) 0
  , vertex (470, 185) 0
  , vertex (470, 205) 0
  ]
  where
    vertex (a, b) c = (Sized.fromTuple (a, b, 0), useBits @Word32 c)

type WorldSize = ("world width" ::: Float, "world height" ::: Float)

type GraphicsUboContents = ("time" ::: Float,
  ("window width" ::: Int32, "window height" ::: Int32),
  WorldSize)
type instance UboInput Graphics = ("window width" ::: Int32, "window height" ::: Int32)

graphicsUboData :: MonadIO m => m (UboData Graphics)
graphicsUboData = MkUboData proxy# (\(w, h) (i, (_, _), world) -> (i + 1, (w, h), world)) <$>
  newIORef @_ @GraphicsUboContents (0, (0, 0), (worldWidth, worldHeight))

-- dt is in femtoseconds
type ComputeUboContents = ("dt" ::: Float, WorldSize)
type instance UboInput Compute = ()

computeUboData :: MonadIO m => m (UboData Compute)
computeUboData = MkUboData proxy# (const id) <$>
  newIORef @_ @ComputeUboContents (0.1, (worldWidth, worldHeight))

setupComputeCommands :: (MonadIO m, HasLogger, HasVulkanResources) => m ()
setupComputeCommands = do
  mutables <- readRes ?computeMutables
  useCommandBuffer mutables.commandBuffer zero{flags = COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT} do
    -- TODO: consider having VERTEX_SHADER_BIT as dstStageMask specifically for
    -- updPos (or possibly for all shaders for convenience) if it doesn't
    -- negatively impact performance
    let memoryBarrier = zero{ srcAccessMask = ACCESS_SHADER_WRITE_BIT
                            , dstAccessMask = ACCESS_SHADER_READ_BIT
                            } :: MemoryBarrier
        stageMask = PIPELINE_STAGE_TRANSFER_BIT .|. PIPELINE_STAGE_COMPUTE_SHADER_BIT
    cmdBindDescriptorSets mutables.commandBuffer PIPELINE_BIND_POINT_COMPUTE
                          mutables.pipelineLayout 0 mutables.descriptorSets []
    for_ (Sized.toList mutables.pipelines) \pipeline -> do
      cmdPipelineBarrier mutables.commandBuffer stageMask stageMask zero [memoryBarrier] [] []
      cmdBindPipeline mutables.commandBuffer PIPELINE_BIND_POINT_COMPUTE pipeline
      -- TODO get the size by checking local size and number of particles
      cmdDispatch mutables.commandBuffer 3 1 1

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
                                         , format = FORMAT_R32G32B32_SFLOAT
                                         , offset = offset0
                                         } :: VertexInputAttributeDescription
                                  , zero { binding = 0
                                         , location = 1
                                         , format = FORMAT_R32_SFLOAT
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
    -- storage bindings for all storage buffers + the vertex storage buffer
    storageBindings = [0..integralNatVal @ComputeStorageBufferCount] <&> \binding ->
      zero{ binding
          , descriptorType = DESCRIPTOR_TYPE_STORAGE_BUFFER
          , descriptorCount = 1
          , stageFlags = SHADER_STAGE_COMPUTE_BIT
          }

vulkanConfig :: MonadIO m => m (Dict HasVulkanConfig)
vulkanConfig = do
  graphicsUboData'@(MkUboData graphicsUboProxy _ _) <- graphicsUboData
  computeUboData'@(MkUboData computeUboProxy _ _) <- computeUboData
  let ?graphicsPipelineLayoutInfo      = graphicsPipelineLayoutInfo
      ?computePipelineLayoutInfo       = computePipelineLayoutInfo
      ?vertexInputInfo                 = vertexInputInfo
      ?vertexBufferInfo                = vertexBufferInfo
      ?vertexData                      = MkVertexData VulkanConfig.Pipeline.vertexData
      ?graphicsDescriptorSetLayoutInfo = graphicsDescriptorSetLayoutInfo
      ?computeDescriptorSetLayoutInfo  = computeDescriptorSetLayoutInfo
      ?graphicsUniformBufferSize       = fromIntegral $ sizeOfProxied graphicsUboProxy
      ?graphicsUboData                 = graphicsUboData'
      ?computeUniformBufferSize        = fromIntegral $ sizeOfProxied computeUboProxy
      ?computeUboData                  = computeUboData'
      ?computeStorageData              = computeStorageData
      ?desiredSwapchainImageNum        = 3
  pure Dict
