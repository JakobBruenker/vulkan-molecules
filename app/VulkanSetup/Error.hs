{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-# LANGUAGE NoFieldSelectors #-}

module VulkanSetup.Error where

import RIO
import RIO.Text qualified as T
import RIO.ByteString qualified as B

import GHC.Stack (callStack)
import Vulkan ((:::))

data AppException
  = GLFWInitError
  | GLFWWindowError
  | GLFWVulkanNotSupported
  | VkValidationLayersNotSupported (NonEmpty ByteString)
  | VkNoPhysicalDevices
  | VkNoSuitableDevices
  | VkWrongNumberOfCommandBuffers ("expected" ::: Natural) ("actual" ::: Natural)
  | VkWrongNumberOfGraphicsPipelines ("expected" ::: Natural) ("actual" ::: Natural)
  | VkWrongNumberOfComputePipelines ("expected" ::: Natural) ("actual" ::: Natural)
  | VkCommandBufferIndexOutOfRange
  | VkUniformBufferIndexOutOfRange
  | VkNoSuitableMemoryType

instance Show AppException where
  show = T.unpack . textDisplay

instance Display AppException where
  display = \case
    GLFWInitError -> "Failed to initialize GLFW."

    GLFWWindowError -> "Failed to create window."

    GLFWVulkanNotSupported -> "GLFW failed to find Vulkan loader or ICD."

    VkValidationLayersNotSupported ls ->
      "Requested validation layer" <> num <> " not supported: " <>
        (displayBytesUtf8 . B.intercalate ", " . toList) ls <> "."
      where num | _ :| [] <- ls = " is"
                | otherwise     = "s are"

    VkNoPhysicalDevices -> "No physical devices found."

    VkNoSuitableDevices -> "No suitable devices found."

    VkWrongNumberOfCommandBuffers expected actual ->
      "Wrong number of graphics pipelines was created: Expected " <>
      displayShow expected <> " but got " <> displayShow actual <> "."

    VkWrongNumberOfGraphicsPipelines expected actual ->
      "Wrong number of graphics pipelines was created: Expected " <>
      displayShow expected <> " but got " <> displayShow actual <> "."

    VkWrongNumberOfComputePipelines expected actual ->
      "Wrong number of compute pipelines was created: Expected " <>
      displayShow expected <> " but got " <> displayShow actual <> "."

    VkCommandBufferIndexOutOfRange -> "Vulkan requested a command buffer with" <>
      " a higher index than was allocated."

    VkUniformBufferIndexOutOfRange -> "Program requested a uniform buffer with" <>
      " a higher index than was allocated."

    VkNoSuitableMemoryType -> "Coludn't find a suitable memory type for Vulkan buffer creation."

data AppExceptionWithCallStack = AppExceptionWithCallStack
  { appException :: AppException
  , callStack :: CallStack
  } deriving Show

instance Exception AppExceptionWithCallStack

throw :: HasCallStack => MonadIO m => AppException -> m a
throw e = throwIO $ AppExceptionWithCallStack e callStack
