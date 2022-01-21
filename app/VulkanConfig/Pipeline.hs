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

import Types
import Utils

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
        cmdDraw commandBuffer 3 1 0 0
  logDebug "Set up commands."

graphicsPipelineLayoutInfo :: PipelineLayoutCreateInfo
graphicsPipelineLayoutInfo = zero
