{-# LANGUAGE OverloadedLists #-}

module Main (main) where

import RIO hiding (logDebug, logInfo, logWarn, logError)

import Options.Applicative ((<**>), fullDesc, info, execParser, helper)
import Control.Monad.Trans.Resource (runResourceT, ResIO)
import Data.Finite (Finite, natToFinite)
import Data.Tuple (swap)
import Data.Vector.Storable.Sized qualified as Sized
import Control.Monad.Extra (fromMaybeM, whenJust)
import Control.Lens (ix, (??), both)
import Foreign (castPtr, with, copyBytes)
import Control.Concurrent (forkIO)
import Control.Monad (replicateM)
import System.IO

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

import VulkanConfig.Pipeline as PL
import VulkanConfig.Shaders
import VulkanSetup.Initialize
import VulkanSetup.GraphicsMutables
import VulkanSetup.Types
import Options
import Utils
import Types
import VulkanSetup.Error

-- TODO use low latency garbage collector
main :: IO ()
main = do
  -- On windows, user needs to set `chcp.com 65001`
  let encoding = utf8
  hSetEncoding stdout encoding
  hSetEncoding stderr encoding
  opts <- execParser (info (options <**> helper) fullDesc)
  -- TODO make color configurable, or maybe check docs on hIsTerminalDevice
  logOptions <- setLogUseColor True <$> logOptionsHandle stderr (optVerbose opts)
  withLogFunc logOptions \logFunc -> do
    let ?logFunc = logFunc
    Dict <- pure $ mkConfig opts
    catch runApp \(e :: SomeException) -> do
      logError $ display e
      exitFailure

runApp :: (HasLogger, HasConfig) => IO ()
runApp = runResourceT do
  Dict <- pure shaderPaths
  Dict <- vulkanConfig
  Dict <- initializeVulkan setupGraphicsCommands setupComputeCommands

  killCompute <- newEmptyMVar
  continueCompute <- newMVar ()

  let ?killCompute = killCompute
      ?continueCompute = continueCompute

  liftIO $ GLFW.setMouseButtonCallback ?window $ Just mouseButtonCallback
  liftIO $ GLFW.setKeyCallback ?window $ Just keyCallback

  for_ @[] [0..integralNatVal @MaxFramesInFlight - 1] copyGraphicsUniformBuffer
  copyComputeUniformBuffer

  mainLoop

  logInfo "Goodbye!"

mouseButtonCallback :: HasVulkanResources => GLFW.MouseButtonCallback
mouseButtonCallback _ _ state _ | state == GLFW.MouseButtonState'Pressed = updateComputeUniformBuffer
                                | otherwise                              = pure ()

keyCallback :: (HasWindow, HasContinueCompute) => GLFW.KeyCallback
keyCallback _ key _ GLFW.KeyState'Pressed _ = case key of
  GLFW.Key'Space ->
    -- basically, fill the MVar if it was empty and vice versa
    unlessM (tryPutMVar ?continueCompute ()) $ takeMVar ?continueCompute
  GLFW.Key'Escape -> GLFW.setWindowShouldClose ?window True
  _ -> pure ()
keyCallback _ _ _ _ _ = pure ()

data ShouldRecreateSwapchain = Don'tRecreate
                             | PleaseRecreate
                             deriving Eq

mainLoop :: (HasLogger, HasVulkanResources, HasContinueCompute, HasKillCompute) => ResIO ()
mainLoop = do
  logDebug "Starting main loop."

  -- putting the MVar twice at the end, once to signal that the thread should
  -- be killed, and once to wait for the thread to be done.
  bracket_ (liftIO $ forkIO enqueueCompute) (replicateM 2 $ putMVar ?killCompute ()) do
    fix ?? natToFinite (Proxy @0) $ \loop currentFrame -> do
      -- NB: depending on present mode and possibly other factors, the exception
      -- is thrown either by 'acquireNextImageKHR' or by 'queuePresentKHR'
      shouldRecreate <- drawFrame currentFrame `catch` \case
        VulkanException ERROR_OUT_OF_DATE_KHR -> pure PleaseRecreate
        ex -> throw $ VkUnexpectedExceptionWhileDrawingFrame ex
      resized <- readIORef ?framebufferResized
      when (resized || shouldRecreate == PleaseRecreate) $ recreateSwapchain setupGraphicsCommands
      -- TODO This is not a reliable way to get a consistent framerate
      threadDelay (1e6 `div` 60)
      liftIO GLFW.pollEvents

      unlessM ?? loop (currentFrame + 1) $ liftIO $ GLFW.windowShouldClose ?window

  -- Allow queues/buffers to finish their job
  deviceWaitIdle ?device

-- TODO benchmark to see if changing this makes any difference
enqueuesPerStep :: Natural
enqueuesPerStep = 10

-- This first enqueues a bunch A of commands, then immediately enqueues a bunch
-- B, then waits until A is done before enqueuing A again, etc.
-- This lets us ensure that there are always some commands in the queue.
-- I'm not sure if that's actually necessary, compared to just using a single fence,
-- but it seems reasonable.
enqueueCompute :: (HasDevice, HasComputeMutables, HasComputeFences, HasComputeQueue,
                   HasContinueCompute, HasKillCompute)
               => IO ()
enqueueCompute = do
  computeMutables <- readRes ?computeMutables
  let submitInfo = pure . SomeStruct $
        zero{commandBuffers = [computeMutables.commandBuffer.commandBufferHandle]}
  go submitInfo ?computeFences

  where
    go submitInfo fences@(fence, _) = do
      _result <- waitForFencesSafe ?device [fence] False maxBound
      resetFences ?device [fence]
      replicateM_ (fromIntegral enqueuesPerStep - 1) $ queueSubmit ?computeQueue submitInfo NULL_HANDLE
      queueSubmit ?computeQueue submitInfo fence
      (race_ `on` readMVar) ?continueCompute ?killCompute
      unlessM (isJust <$> tryTakeMVar ?killCompute) $ go submitInfo (swap fences)

drawFrame :: HasVulkanResources => Finite MaxFramesInFlight -> ResIO ShouldRecreateSwapchain
drawFrame currentFrame = do
  mutables <- readRes ?graphicsMutables

  let ixSync :: Storable a => Sized.Vector MaxFramesInFlight a -> a
      ixSync = view $ Sized.ix currentFrame

  _result <- waitForFencesSafe ?device [ixSync ?inFlight] True maxBound

  (imageResult, imageIndex) <-
    acquireNextImageKHR ?device mutables.swapchain maxBound (ixSync ?imageAvailable) NULL_HANDLE

  imageRelated <- fromMaybeM
    do throw VkCommandBufferIndexOutOfRange
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
  modifyIORef' ref $ update (windowWidth, windowHeight)
  copyGraphicsUniformBuffer currentImageIndex

copyGraphicsUniformBuffer :: (MonadUnliftIO m, HasDevice, HasGraphicsUboData,
                                HasGraphicsUniformBuffers, HasGraphicsUniformBufferSize)
                            => Word32 -> m ()
copyGraphicsUniformBuffer imageIndex = do
  MkUboData{ref} <- pure ?graphicsUboData
  time <- readIORef ref
  memory <- maybe
    do throw VkUniformBufferIndexOutOfRange
    do pure . snd
    do ?graphicsUniformBuffers ^? ix (fromIntegral imageIndex)
  withMappedMemory ?device memory 0 ?graphicsUniformBufferSize zero bracket \target ->
    liftIO $ with time \(castPtr -> source) ->
      copyBytes target source $ fromIntegral ?graphicsUniformBufferSize

updateComputeUniformBuffer :: (MonadUnliftIO m, HasDevice,
                               HasComputeUboData, HasComputeUniformBuffer, HasComputeUniformBufferSize)
                           => m ()
updateComputeUniformBuffer = do
  MkUboData{update, ref} <- pure ?computeUboData
  modifyIORef' ref $ update ()
  copyComputeUniformBuffer

copyComputeUniformBuffer :: (MonadUnliftIO m, HasDevice,
                             HasComputeUboData, HasComputeUniformBuffer, HasComputeUniformBufferSize)
                         => m ()
copyComputeUniformBuffer = do
  MkUboData{ref} <- pure ?computeUboData
  subIndex <- readIORef ref
  withMappedMemory
    ?device (snd ?computeUniformBuffer) 0 ?computeUniformBufferSize zero bracket \target ->
      liftIO $ with subIndex \(castPtr -> source) ->
        copyBytes target source $ fromIntegral ?computeUniformBufferSize
