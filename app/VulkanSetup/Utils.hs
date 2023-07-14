module VulkanSetup.Utils where

import RIO hiding (logWarn)
import RIO.Text qualified as T
import RIO.FilePath
import RIO.NonEmpty qualified as NE
import Data.List.Extra
import Data.Tuple (swap)

import VulkanSetup.Types

import Vulkan hiding ( MacOSSurfaceCreateInfoMVK(view)
                     , IOSSurfaceCreateInfoMVK(view)
                     , Display
                     , WaylandSurfaceCreateInfoKHR(display)
                     , DisplayPropertiesKHR(display)
                     )
import Vulkan.Zero
import Vulkan.Utils.ShaderQQ.GLSL.Shaderc (compileShader)
import Utils (logWarn)
import VulkanSetup.Error

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

nonEmptyInits :: NonEmpty a -> NonEmpty (NonEmpty a)
nonEmptyInits (x:|xs) = (x :|) <$> NE.scanl snoc [] xs

withShader :: (MonadUnliftIO m, HasLogger, HasDevice) => FilePath -> (ShaderModule -> m r) -> m r
withShader path action = do
  code <- readFileUtf8 path
  (warnings, result) <- compileShader Nothing Nothing (drop 1 $ takeExtension path) (T.unpack code)
  traverse_ (logWarn . displayShow) warnings
  bytes <- case result of
    Left errs -> throw $ GLSLCompilationErrors path errs
    Right bytes -> pure bytes
  withShaderModule ?device zero{code = bytes} Nothing bracket action

withShaders :: (MonadUnliftIO m, HasLogger, HasDevice) => [FilePath] -> ([ShaderModule] -> m r) -> m r
withShaders = go []
  where
    go acc [] action = action $ reverse acc
    go acc (path:paths) action = withShader path \shader -> go (shader:acc) paths action
