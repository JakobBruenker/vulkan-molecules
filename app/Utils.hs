module Utils where

import RIO
import Data.Tuple (swap)

import Control.Monad.Trans.Resource (ReleaseKey, MonadResource)

import Types

traverseToSnd :: Functor f => (a -> f b) -> a -> f (a, b)
traverseToSnd = liftA2 fmap (,)

traverseToFst :: Functor f => (a -> f b) -> a -> f (b, a)
traverseToFst = (fmap swap .) . traverseToSnd

plural :: (Semigroup a, IsString a, Eq b, Num b) => a -> b -> a
plural str n = str <> bool "s" "" (n == 1)

-- in base since 4.16
clamp :: Ord a => (a, a) -> a -> a
clamp (low, high) a = min high (max a low)

mkMResource :: MonadResource m => (ReleaseKey, a) -> m (MResource a)
mkMResource = newIORef . uncurry MkResource

viewRef :: (MonadReader s m, MonadIO m) => Getting (IORef a) s (IORef a) -> m a
viewRef l = readIORef =<< view l

viewRes :: (MonadIO m, MonadReader s m) => Getting (MResource a) s (MResource a) -> m a
viewRes l = view resourceL <$> (readIORef =<< view l)

(^->) :: MonadIO m => IORef s -> Getting a s a -> m a
x^->l = view l <$> readIORef x
