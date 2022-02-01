{-# LANGUAGE OverloadedLists #-}

module VulkanSetup.ComputeMutables where

import RIO hiding (logDebug, logInfo, logWarn, logError)
import RIO.Vector qualified as V
import RIO.Vector.Partial qualified as V -- TODO get rid of this import

import Control.Monad.Trans.Resource (allocate, ReleaseKey, ResIO)
import Data.Vector.Storable.Sized qualified as Sized
import Data.Vector.Sized qualified as Sized'
import GHC.TypeNats (KnownNat)
import Foreign.Storable.Tuple ()
import Data.Finite (finites)
import Foreign (sizeOf)

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

initComputeMutables :: (HasLogger, HasComputeShaderPaths, HasVulkanConfig,
                       HasDevice, HasComputeDescriptorSetLayouts, HasComputeUniformBuffer,
                       HasVertexStorageBuffer, HasComputeCommandPool, HasComputeStorageBuffers,
                       KnownNat ComputeShaderCount, KnownNat ComputeStorageBufferCount)
                     => ResIO (Dict HasComputeMutables)
initComputeMutables = do
  computeMutables <- mkMResources =<< constructComputeMutables
  let ?computeMutables = computeMutables
  pure Dict

constructComputeMutables :: (HasLogger, HasComputeShaderPaths, HasVulkanConfig,
                             HasDevice, HasComputeDescriptorSetLayouts, HasComputeUniformBuffer,
                             HasVertexStorageBuffer, HasComputeCommandPool, HasComputeStorageBuffers,
                             KnownNat ComputeShaderCount, KnownNat ComputeStorageBufferCount)
                          => ResIO ([ReleaseKey], ComputeMutables)
constructComputeMutables = do
  (layoutKey    , pipelineLayout ) <- constructComputePipelineLayout
  (pipelineKey  , pipelines      ) <- constructComputePipelines pipelineLayout
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
                            HasVertexStorageBuffer, HasVertexBufferInfo, HasComputeStorageBuffers,
                            KnownNat ComputeStorageBufferCount, HasComputeStorageData)
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
                                , range = ?computeUniformBufferSize
                                } :: DescriptorBufferInfo
                          ]
  let descriptorWrites = concat $ (\f -> V.zipWith f ?computeDescriptorSetLayoutInfo descriptorSets)
        \layoutInfo descriptorSet -> let type' = (V.head layoutInfo.bindings).descriptorType in do
          let descriptorWrite = zero{ dstSet = descriptorSet
                                    , dstBinding = 0
                                    , dstArrayElement = 0
                                    , descriptorType = type'
                                    , descriptorCount = 1
                                    } :: WriteDescriptorSet '[]
          if type' == DESCRIPTOR_TYPE_UNIFORM_BUFFER
          then [SomeStruct descriptorWrite{bufferInfo = uniformBufferInfo}]
          -- binding vertex buffer as first storage buffer
          else [ SomeStruct descriptorWrite
                   { bufferInfo = [ zero{ buffer = ?vertexStorageBuffer
                                        , range = ?vertexBufferInfo.size
                                        } :: DescriptorBufferInfo
                                  ]
                   }
               ] <> do
                 -- then binding all other storage buffers
                 finites @ComputeStorageBufferCount <&> \binding ->
                   case ?computeStorageData^.Sized'.ix binding of
                      MkStorageData dat -> do
                        let bufferInfo = [
                              zero{ buffer = ?computeStorageBuffers^.Sized'.ix binding
                                  , range = fromIntegral $ sizeOf dat
                                  } :: DescriptorBufferInfo]
                        SomeStruct descriptorWrite{bufferInfo, dstBinding = fromIntegral binding + 1}
  updateDescriptorSets ?device (V.fromList descriptorWrites) []
  pure (poolKey, descriptorSets)

constructComputePipelineLayout :: (HasDevice, HasComputeDescriptorSetLayouts,
                                   HasComputePipelineLayoutInfo)
                                => ResIO (ReleaseKey, PipelineLayout)
constructComputePipelineLayout = withPipelineLayout
  ?device ?computePipelineLayoutInfo{setLayouts = ?computeDescriptorSetLayouts} Nothing allocate

constructComputePipelines :: (HasLogger, HasDevice, HasComputeShaderPaths, KnownNat ComputeShaderCount)
                          => PipelineLayout
                          -> ResIO (ReleaseKey, Sized.Vector ComputeShaderCount Pipeline)
constructComputePipelines layout = do
  (releasePipelines, (_, pipelineList)) <-
    withShaders (toList ?computeShaderPaths) \modules -> do

      let pipelineInfos = V.fromList $ modules <&> \module' ->
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

recreateComputePipeline :: (HasLogger, HasVulkanResources, HasVertexStorageBuffer,
                            KnownNat ComputeShaderCount, KnownNat ComputeStorageBufferCount)
                        => (HasVulkanResources => ResIO ()) -> ResIO ()
recreateComputePipeline setupCommands = do
  logDebug "Recreating compute pipeline..."

  writeIORef ?framebufferResized False

  deviceWaitIdle ?device

  writeRes ?computeMutables constructComputeMutables

  setupCommands
