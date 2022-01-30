{-# LANGUAGE OverloadedLists #-}

module VulkanSetup.ComputeMutables where

import RIO hiding (logDebug, logInfo, logWarn, logError)
import qualified RIO.Vector as V
import qualified RIO.Vector.Partial as V -- TODO get rid of this import

import Control.Monad.Trans.Resource (allocate, ReleaseKey, ResIO)
import Foreign.Storable.Tuple ()

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

initComputeMutables :: (HasLogger, HasComputeShaderPath, HasVulkanConfig,
                       HasDevice, HasComputeDescriptorSetLayouts, HasComputeUniformBuffer,
                       HasComputeStorageBuffer, HasComputeCommandPool)
                     => ResIO (Dict HasComputeMutables)
initComputeMutables = do
  computeMutables <- mkMResources =<< constructComputeMutables
  let ?computeMutables = computeMutables
  pure Dict

constructComputeMutables :: (HasLogger, HasComputeShaderPath, HasVulkanConfig,
                             HasDevice, HasComputeDescriptorSetLayouts, HasComputeUniformBuffer,
                             HasComputeStorageBuffer, HasComputeCommandPool)
                          => ResIO ([ReleaseKey], ComputeMutables)
constructComputeMutables = do
  (layoutKey    , pipelineLayout ) <- constructComputePipelineLayout
  (pipelineKey  , pipeline       ) <- constructComputePipeline pipelineLayout
  (dsKey        , descriptorSets ) <- constructDescriptorSets
  (cmdBufKey    , commandBuffer  ) <- constructCommandBuffer

  pure ([layoutKey, pipelineKey, dsKey, cmdBufKey], MkComputeMutables{..})

constructCommandBuffer :: (HasDevice, HasComputeCommandPool)
                       => ResIO (ReleaseKey, CommandBuffer)
constructCommandBuffer = do
  let commandBuffersInfo = zero{ commandPool = ?computeCommandPool
                               , level = COMMAND_BUFFER_LEVEL_PRIMARY
                               , commandBufferCount = 1
                               }
  withCommandBuffers ?device commandBuffersInfo allocate >>= \case
    (key, [buffer]) -> pure (key, buffer)
    (_, buffers) -> throwIO $ VkWrongNumberOfCommandBuffers 1 (fromIntegral $ V.length buffers)

constructDescriptorSets :: (HasDevice, HasComputeUniformBuffer, HasComputeDescriptorSetLayoutInfo,
                            HasComputeDescriptorSetLayouts, HasComputeUniformBufferSize,
                            HasComputeStorageBuffer, HasVertexBufferInfo)
                        => ResIO (ReleaseKey, Vector DescriptorSet)
constructDescriptorSets = do
  let poolSizes = ?computeDescriptorSetLayoutInfo <&> \layoutInfo ->
        zero{ descriptorCount = sum (layoutInfo.bindings <&> \b -> b.descriptorCount)
            , type' = (V.head layoutInfo.bindings).descriptorType
            }
      poolInfo = zero{poolSizes, maxSets = 2}
  (poolKey, descriptorPool) <- withDescriptorPool ?device poolInfo Nothing allocate

  let allocInfo = zero{descriptorPool, setLayouts = ?computeDescriptorSetLayouts}
  descriptorSets <- allocateDescriptorSets ?device allocInfo

  let uniformBufferInfo = [ zero{ buffer = fst ?computeUniformBuffer
                                , offset = 0
                                , range = ?computeUniformBufferSize
                                } :: DescriptorBufferInfo
                          ]
      storageBufferInfo = [ zero{ buffer = ?computeStorageBuffer
                                , offset = 0
                                , range = ?vertexBufferInfo.size
                                } :: DescriptorBufferInfo
                          ]
  -- TODO this isn't really fully general yet for other descriptor set layouts
  let descriptorWrites = (\f -> V.zipWith f ?computeDescriptorSetLayoutInfo descriptorSets)
        \layoutInfo descriptorSet -> let type' = (V.head layoutInfo.bindings).descriptorType in
           SomeStruct zero{ dstSet = descriptorSet
                          , dstBinding = 0
                          , dstArrayElement = 0
                          , descriptorType = type'
                          , descriptorCount = 1
                          , bufferInfo = if type' == DESCRIPTOR_TYPE_UNIFORM_BUFFER
                                         then uniformBufferInfo
                                         else storageBufferInfo
                          } :: SomeStruct WriteDescriptorSet
  updateDescriptorSets ?device descriptorWrites []
  pure (poolKey, descriptorSets)

constructComputePipelineLayout :: (HasDevice, HasComputeDescriptorSetLayouts,
                                   HasComputePipelineLayoutInfo)
                                => ResIO (ReleaseKey, PipelineLayout)
constructComputePipelineLayout = withPipelineLayout
  ?device ?computePipelineLayoutInfo{setLayouts = ?computeDescriptorSetLayouts} Nothing allocate

constructComputePipeline :: (HasLogger, HasDevice, HasComputeShaderPath)
                         => PipelineLayout -> ResIO (ReleaseKey, Pipeline)
constructComputePipeline layout = do
  (releasePipelines, (_, pipelines)) <-
    withShader ?computeShaderPath \computeModule -> do

      let computeShaderStageInfo = zero{ stage = SHADER_STAGE_COMPUTE_BIT
                                       , module' = computeModule
                                       , name = "main"
                                       }
          shaderStage = SomeStruct computeShaderStageInfo
          pipelineInfo = pure $ SomeStruct zero{ stage = shaderStage
                                               , layout
                                               }

      withComputePipelines ?device zero pipelineInfo Nothing allocate

  graphicsPipeline <- case pipelines of
    [pipeline] -> pure (releasePipelines, pipeline)
    (length -> num) -> throwIO $ VkWrongNumberOfGraphicsPipelines 1 $ fromIntegral num

  logDebug "Created compute pipeline."
  pure graphicsPipeline

recreateComputePipeline :: (HasLogger, HasVulkanResources, HasComputeResources,
                            HasComputeStorageBuffer)
                        => (HasVulkanResources => ResIO ()) -> ResIO ()
recreateComputePipeline setupCommands = do
  logDebug "Recreating compute pipeline..."

  writeIORef ?framebufferResized False

  deviceWaitIdle ?device

  writeRes ?computeMutables constructComputeMutables

  setupCommands
