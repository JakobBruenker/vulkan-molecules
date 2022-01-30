{-# LANGUAGE OverloadedLists #-}

module Main (main) where

import RIO hiding (logDebug, logInfo, logWarn, logError)

import Options.Applicative
import Control.Monad.Trans.Resource (runResourceT, ResIO)
import Data.Finite (Finite, natToFinite)
import qualified Data.Vector.Storable.Sized as Sized
import Control.Monad.Extra (fromMaybeM, whenJust)
import Control.Lens (ix, (??), both)
import Data.Tuple.Extra (dupe)
import Foreign (castPtr, with, copyBytes)

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
import VulkanSetup.Initialize
import VulkanSetup.GraphicsMutables
import VulkanSetup.Types
import Options
import Utils

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
  Dict <- initializeVulkan setupGraphicsCommands setupComputeCommands

  mainLoop

  logInfo "Goodbye!"

data ShouldRecreateSwapchain = Don'tRecreate
                             | PleaseRecreate
                             deriving Eq

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
    -- TODO I don't think this is actually a reliable way to set the framerate
    liftIO $ GLFW.waitEventsTimeout 0.004

    unlessM ?? loop (currentFrame + 1) $ liftIO $ GLFW.windowShouldClose ?window

  -- Allow queues/buffers to finish their job
  deviceWaitIdle ?device

drawFrame :: HasVulkanResources => Finite MaxFramesInFlight -> ResIO ShouldRecreateSwapchain
drawFrame currentFrame = do
  -- compute stuff
  computeMutables <- readRes ?computeMutables
  let computeSubmitInfo = pure . SomeStruct $
        zero{commandBuffers = [computeMutables.commandBuffer.commandBufferHandle]}
  queueSubmit ?computeQueue computeSubmitInfo NULL_HANDLE


  -- graphics stuff
  mutables <- readRes ?graphicsMutables

  let ixSync :: Storable a => Sized.Vector MaxFramesInFlight a -> a
      ixSync = view $ Sized.ix currentFrame

  _result <- waitForFences ?device [ixSync ?inFlight] True maxBound

  (imageResult, imageIndex) <-
    acquireNextImageKHR ?device mutables.swapchain maxBound (ixSync ?imageAvailable) NULL_HANDLE

  imageRelated <- fromMaybeM
    do throwIO VkCommandBufferIndexOutOfRange
    do pure $ mutables.imageRelateds ^? ix (fromIntegral imageIndex)

  imageInFlightFence <- readIORef imageRelated.imageInFlight

  -- Check if a previous frame is using this image (i.e. there is its fence to wait on)
  whenJust imageInFlightFence \fence -> void $ waitForFences ?device [fence] True maxBound

  -- Mark the image as now being in use by this frame
  writeIORef imageRelated.imageInFlight (Just $ ixSync ?inFlight)

  (windowWidth, windowHeight) <- over both fromIntegral <$> liftIO (GLFW.getFramebufferSize ?window)
  updateGraphicsUniformBuffer imageIndex windowWidth windowHeight

  let submitInfo = pure . SomeStruct $
        zero{ waitSemaphores = [ixSync ?imageAvailable]
            , waitDstStageMask = [PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT]
            , commandBuffers = [imageRelated.commandBuffer.commandBufferHandle]
            , signalSemaphores = [ixSync ?renderFinished]
            }

  resetFences ?device [ixSync ?inFlight]
  queueSubmit ?graphicsQueue submitInfo (ixSync ?inFlight)

  let presentInfo = zero{ waitSemaphores = [ixSync ?renderFinished]
                        , swapchains = [mutables.swapchain]
                        , imageIndices = [imageIndex]
                        , results = zero
                        }
  queueResult <- queuePresentKHR ?presentQueue presentInfo

  pure if | elem @[] SUBOPTIMAL_KHR [imageResult, queueResult] -> PleaseRecreate
          | otherwise -> Don'tRecreate

updateGraphicsUniformBuffer :: (MonadUnliftIO m, HasDevice, HasGraphicsUboData,
                                HasGraphicsUniformBuffers, HasGraphicsUniformBufferSize)
                            => Word32 -> Int32 -> Int32 -> m ()
updateGraphicsUniformBuffer currentImageIndex windowWidth windowHeight = do
  MkUboData{update, ref} <- pure ?graphicsUboData
  time <- atomicModifyIORef' ref (dupe . update (windowWidth, windowHeight))
  memory <- maybe
    do throwIO VkUniformBufferIndexOutOfRange
    do pure . snd
    do ?graphicsUniformBuffers ^? ix (fromIntegral currentImageIndex)
  withMappedMemory ?device memory 0 ?graphicsUniformBufferSize zero bracket \target ->
    liftIO $ with time \(castPtr -> source) ->
      copyBytes target source $ fromIntegral ?graphicsUniformBufferSize
