module VulkanSetup.Utils where

import RIO hiding (logWarn)
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
import Control.Monad.Trans.Maybe (MaybeT(..))

traverseToSnd :: Functor f => (a -> f b) -> a -> f (a, b)
traverseToSnd = liftA2 fmap (,)

traverseToFst :: Functor f => (a -> f b) -> a -> f (b, a)
traverseToFst = (fmap swap .) . traverseToSnd

plural :: (Semigroup a, IsString a, Eq b, Num b) => a -> b -> a
plural str n = str <> bool "s" "" (n == 1)

viewRef :: (MonadReader s m, MonadIO m) => Getting (IORef a) s (IORef a) -> m a
viewRef l = readIORef =<< view l

nonEmptyInits :: NonEmpty a -> NonEmpty (NonEmpty a)
nonEmptyInits (x:|xs) = (x :|) <$> NE.scanl snoc [] xs

withShader :: (MonadUnliftIO m, HasDevice) => FilePath -> (ShaderModule -> m r) -> m r
withShader path action = do
  withShaderModule ?device zero{code = !(readFileBinary path)} Nothing bracket action

withShaders :: (MonadUnliftIO m, HasDevice) => [FilePath] -> ([ShaderModule] -> m r) -> m r
withShaders = go []
  where
    go acc [] action = action $ reverse acc
    go acc (path:paths) action = withShader path \shader -> go (shader:acc) paths action
    
joinMaybeT :: Functor m => MaybeT m (Maybe a) -> MaybeT m a
joinMaybeT = MaybeT . fmap join . runMaybeT
