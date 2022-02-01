{-# LANGUAGE OverloadedLists #-}

module VulkanSetup.ComputeMutables where

import RIO hiding (logDebug, logInfo, logWarn, logError)
import qualified RIO.Vector as V
import qualified RIO.Vector.Partial as V -- TODO get rid of this import

import Control.Monad.Trans.Resource (allocate, ReleaseKey, ResIO)
import qualified Data.Vector.Sized as Sized
import GHC.TypeNats (KnownNat)
import Foreign.Storable.Tuple ()

import Vulkan
    ( withCommandBuffers,
      allocateDescriptorSets,
      updateDescriptorSets,
      withDescriptorPool,
      withComputePipelines,
      withPipelineLayout,
      deviceWaitIdle,
      BufferCreateInfo(size),
      CommandBufferAllocateInfo(commandBufferCount, commandPool, level),
      DescriptorBufferInfo(range, buffer, offset),
      DescriptorPoolCreateInfo(maxSets, poolSizes),
      DescriptorPoolSize(type', descriptorCount),
      DescriptorSetAllocateInfo(setLayouts, descriptorPool),
      DescriptorSetLayoutBinding(descriptorCount, descriptorType),
      DescriptorSetLayoutCreateInfo(bindings),
      WriteDescriptorSet(descriptorCount, bufferInfo, dstSet, dstBinding,
                         dstArrayElement, descriptorType),
      CommandBufferLevel(COMMAND_BUFFER_LEVEL_PRIMARY),
      DescriptorType(DESCRIPTOR_TYPE_UNIFORM_BUFFER),
      ShaderStageFlagBits(SHADER_STAGE_COMPUTE_BIT),
      CommandBuffer,
      DescriptorSet,
      Pipeline,
      PipelineLayout,
      ComputePipelineCreateInfo(layout, stage),
      PipelineShaderStageCreateInfo(name, stage, module'),
      PipelineLayoutCreateInfo(setLayouts) )
import Vulkan.Zero
import Vulkan.CStruct.Extends (SomeStruct (SomeStruct))

import Utils
import VulkanSetup.Types
    ( HasVulkanResources,
      HasComputeStorageBuffer,
      HasComputeUniformBuffer,
      HasVulkanConfig,
      HasComputeUniformBufferSize,
      HasVertexBufferInfo,
      HasComputeDescriptorSetLayoutInfo,
      HasComputeShaderPaths,
      ComputeShaderCount,
      HasComputeMutables,
      HasComputeDescriptorSetLayouts,
      HasComputeCommandPool,
      HasDevice,
      HasLogger,
      ComputeMutables(..),
      AppException(VkWrongNumberOfComputePipelines,
                   VkWrongNumberOfCommandBuffers),
      Dict(..),
      mkMResources,
      writeRes,
      HasComputePipelineLayoutInfos )
import VulkanSetup.Utils

initComputeMutables :: (HasLogger, HasComputeShaderPaths, HasVulkanConfig,
                       HasDevice, HasComputeDescriptorSetLayouts, HasComputeUniformBuffer,
                       HasComputeStorageBuffer, HasComputeCommandPool,
                       KnownNat ComputeShaderCount)
                     => ResIO (Dict HasComputeMutables)
initComputeMutables = do
  computeMutables <- mkMResources =<< constructComputeMutables
  let ?computeMutables = computeMutables
  pure Dict

constructComputeMutables :: (HasLogger, HasComputeShaderPaths, HasVulkanConfig,
                             HasDevice, HasComputeDescriptorSetLayouts, HasComputeUniformBuffer,
                             HasComputeStorageBuffer, HasComputeCommandPool,
                             KnownNat ComputeShaderCount)
                          => ResIO ([ReleaseKey], ComputeMutables)
constructComputeMutables = do
  (layoutKeys   , pipelineLayouts) <- constructComputePipelineLayouts
  (pipelineKey  , pipelineVec    ) <- constructComputePipelines pipelineLayouts
  (dsKey        , descriptorSets ) <- constructDescriptorSets
  (cmdBufKey    , commandBuffer  ) <- constructCommandBuffer
  let pipelines = Sized.zip pipelineLayouts pipelineVec

  pure (layoutKeys <> [pipelineKey, dsKey, cmdBufKey], MkComputeMutables{..})

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

constructComputePipelineLayouts :: (HasDevice, HasComputeDescriptorSetLayouts,
                                    HasComputePipelineLayoutInfos)
                                 => ResIO ([ReleaseKey], Sized.Vector ComputeShaderCount PipelineLayout)
constructComputePipelineLayouts = first toList . Sized.unzip <$>
  for ?computePipelineLayoutInfos \layoutInfo -> withPipelineLayout
            ?device layoutInfo{setLayouts = ?computeDescriptorSetLayouts} Nothing allocate

constructComputePipelines :: (HasLogger, HasDevice, HasComputeShaderPaths, KnownNat ComputeShaderCount)
                          => Sized.Vector ComputeShaderCount PipelineLayout
                          -> ResIO (ReleaseKey, Sized.Vector ComputeShaderCount Pipeline)
constructComputePipelines layouts = do
  (releasePipelines, (_, pipelineList)) <-
    withShaders ?computeShaderPaths \modules -> do

      let pipelineInfos = Sized.fromSized $ Sized.zip modules layouts <&> \(module', layout) ->
            SomeStruct zero{ stage = SomeStruct zero{ stage = SHADER_STAGE_COMPUTE_BIT
                                                    , module'
                                                    , name = "main"
                                                    }
                           , layout
                           }

      withComputePipelines ?device zero pipelineInfos Nothing allocate

  computePipeline <- case Sized.fromList (toList pipelineList) of
    Just pipelines -> pure (releasePipelines, pipelines)
    (length -> num) -> throwIO $ VkWrongNumberOfComputePipelines 1 $ fromIntegral num

  logDebug "Created compute pipeline."
  pure computePipeline

recreateComputePipeline :: (HasLogger, HasVulkanResources, HasComputeStorageBuffer,
                            KnownNat ComputeShaderCount)
                        => (HasVulkanResources => ResIO ()) -> ResIO ()
recreateComputePipeline setupCommands = do
  logDebug "Recreating compute pipeline..."

  writeIORef ?framebufferResized False

  deviceWaitIdle ?device

  writeRes ?computeMutables constructComputeMutables

  setupCommands
