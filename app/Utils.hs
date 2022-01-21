module Utils where

import RIO
import Data.Tuple (swap)

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

viewRef :: (MonadReader s m, MonadIO m) => Getting (IORef a) s (IORef a) -> m a
viewRef l = readIORef =<< view l

logDebug :: (HasLogger, MonadIO m) => Utf8Builder -> m ()
logDebug = flip runReaderT ?logFunc . RIO.logDebug

logInfo :: (HasLogger, MonadIO m) => Utf8Builder -> m ()
logInfo = flip runReaderT ?logFunc . RIO.logInfo

logWarn :: (HasLogger, MonadIO m) => Utf8Builder -> m ()
logWarn = flip runReaderT ?logFunc . RIO.logWarn

logError :: (HasLogger, MonadIO m) => Utf8Builder -> m ()
logError = flip runReaderT ?logFunc . RIO.logError
