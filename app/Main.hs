{-# LANGUAGE OverloadedLists #-}

module Main (main) where

import RIO hiding (logDebug, logInfo, logWarn, logError)
import RIO.Vector qualified as V

import Options.Applicative
import Control.Monad.Trans.Resource (runResourceT, ResIO)
import Data.Finite (Finite, natToFinite)
import qualified Data.Vector.Storable.Sized as Sized
import Control.Monad.Extra (fromMaybeM, whenJust)
import Control.Lens (ix, (??))
import Data.Tuple.Extra (dupe)

import qualified Graphics.UI.GLFW as GLFW

import Vulkan hiding ( MacOSSurfaceCreateInfoMVK(view)
                     , IOSSurfaceCreateInfoMVK(view)
                     , Display
                     , WaylandSurfaceCreateInfoKHR(display)
                     , DisplayPropertiesKHR(display)
                     , info
                     )
import Vulkan.Exception
import Vulkan.CStruct.Extends
import Vulkan.Zero

import VulkanConfig.Shaders
import VulkanConfig.Pipeline as PL
import Graphics.Initialize
import Graphics.Mutables
import Graphics.Types
import Options
import Utils
import Foreign (castPtr, with, copyBytes)

main :: IO ()
main = do
  opts <- execParser (info (options <**> helper) fullDesc)
  logOptions <- logOptionsHandle stderr (optVerbose opts)
  withLogFunc logOptions \logFunc -> do
    let ?logFunc = logFunc
    Dict <- pure $ mkConfig opts
    catch runApp \e -> do
      logError $ display @SomeException e
      exitFailure

runApp :: (HasLogger, HasConfig) => IO ()
runApp = runResourceT do
  liftIO compileAllShaders
  logDebug "Compiled shaders."

  Dict <- pure shaderPaths
  Dict <- vulkanConfig
  Dict <- initializeVulkan setupGraphicsCommands

  mainLoop

  logInfo "Goodbye!"

mainLoop :: (HasLogger, HasVulkanResources) => ResIO ()
mainLoop = do
  logDebug "Starting main loop."
  fix ?? natToFinite (Proxy @0) $ \loop currentFrame -> do
    -- NB: depending on present mode and possibly other factors, the exception
    -- is thrown either by 'acquireNextImageKHR' or by 'queuePresentKHR'
    shouldRecreate <- drawFrame currentFrame `catch` \case
      VulkanException ERROR_OUT_OF_DATE_KHR -> pure PleaseRecreate
      ex -> throwIO ex
    resized <- readIORef ?framebufferResized
    when (resized || shouldRecreate == PleaseRecreate) $ recreateSwapchain setupGraphicsCommands
    liftIO GLFW.waitEvents

    unlessM ?? loop (currentFrame + 1) $ liftIO $ GLFW.windowShouldClose ?window

  -- Allow queues/buffers to finish their job
  deviceWaitIdle ?device

drawFrame :: HasVulkanResources => Finite MaxFramesInFlight -> ResIO ShouldRecreateSwapchain
drawFrame currentFrame = do
  MkMutables{swapchain, imageRelateds} <- readRes ?mutables

  let ixSync :: Storable a => Sized.Vector MaxFramesInFlight a -> a
      ixSync = view $ Sized.ix currentFrame

  _result <- waitForFences ?device [ixSync ?inFlight] True maxBound

  (imageResult, imageIndex) <-
    acquireNextImageKHR ?device swapchain maxBound (ixSync ?imageAvailable) NULL_HANDLE

  MkImageRelated{commandBuffer, imageInFlight} <- fromMaybeM
    do throwIO VkCommandBufferIndexOutOfRange
    do pure $ imageRelateds^?ix (fromIntegral imageIndex)

  imageInFlightFence <- readIORef imageInFlight

  -- Check if a previous frame is using this image (i.e. there is its fence to wait on)
  whenJust imageInFlightFence \fence -> void $ waitForFences ?device [fence] True maxBound

  -- Mark the image as now being in use by this frame
  writeIORef imageInFlight (Just $ ixSync ?inFlight)

  updateUniformBuffer imageIndex

  let submitInfo = pure . SomeStruct $
        SubmitInfo{ next = ()
                  , waitSemaphores = [ixSync ?imageAvailable]
                  , waitDstStageMask = [PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT]
                  , commandBuffers = [commandBufferHandle commandBuffer]
                  , signalSemaphores = [ixSync ?renderFinished]
                  }

  resetFences ?device [ixSync ?inFlight]
  queueSubmit ?graphicsQueue submitInfo (ixSync ?inFlight)

  let presentInfo = PresentInfoKHR{ next = ()
                                  , waitSemaphores = [ixSync ?renderFinished]
                                  , swapchains = [swapchain]
                                  , imageIndices = [imageIndex]
                                  , results = zero
                                  }
  queueResult <- queuePresentKHR ?presentQueue presentInfo

  pure if | elem @[] SUBOPTIMAL_KHR [imageResult, queueResult] -> PleaseRecreate
          | otherwise -> Don'tRecreate

updateUniformBuffer :: (MonadUnliftIO m, HasDevice, HasUboData, HasUniformBuffers, HasUniformBufferSize)
                    => ("index" ::: Word32) -> m ()
updateUniformBuffer currentImageIndex = do
  MkUboData{uboUpdate, uboRef} <- pure ?uboData
  time <- atomicModifyIORef' uboRef (dupe . uboUpdate)
  memory <- maybe
    do throwIO VkUniformBufferIndexOutOfRange
    do pure . snd
    do ?uniformBuffers^?ix (fromIntegral currentImageIndex)
  withMappedMemory ?device memory 0 ?uniformBufferSize zero bracket \target ->
    liftIO $ with time \(castPtr -> source) ->
      copyBytes target source $ V.length ?uniformBuffers * fromIntegral ?uniformBufferSize
