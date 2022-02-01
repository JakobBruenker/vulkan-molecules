module VulkanSetup.Utils where

import RIO
import Data.Tuple (swap)

import VulkanSetup.Types

import Vulkan hiding ( MacOSSurfaceCreateInfoMVK(view)
                     , IOSSurfaceCreateInfoMVK(view)
                     , Display
                     , WaylandSurfaceCreateInfoKHR(display)
                     , DisplayPropertiesKHR(display)
                     )
import Vulkan.Zero

traverseToSnd :: Functor f => (a -> f b) -> a -> f (a, b)
traverseToSnd = liftA2 fmap (,)

traverseToFst :: Functor f => (a -> f b) -> a -> f (b, a)
traverseToFst = (fmap swap .) . traverseToSnd

plural :: (Semigroup a, IsString a, Eq b, Num b) => a -> b -> a
plural str n = str <> bool "s" "" (n == 1)

-- in base since 4.16
clamp :: Ord a => (a, a) -> a -> a
clamp (low, high) a = min high (max a low)

viewRef :: (MonadReader s m, MonadIO m) => Getting (IORef a) s (IORef a) -> m a
viewRef l = readIORef =<< view l

withShader :: (MonadUnliftIO m, HasDevice) => FilePath -> (ShaderModule -> m r) -> m r
withShader path action = do
  bytes <- readFileBinary path
  withShaderModule ?device zero{code = bytes} Nothing bracket action

withShaders :: (MonadUnliftIO m, HasDevice) => [FilePath] -> ([ShaderModule] -> m r) -> m r
withShaders = go []
  where
    go acc [] action = action $ reverse acc
    go acc (path:paths) action = withShader path \shader -> go (shader:acc) paths action
