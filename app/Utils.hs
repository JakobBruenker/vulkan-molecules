module Utils where

import RIO

import Graphics.Types

logDebug :: (HasLogger, MonadIO m) => Utf8Builder -> m ()
logDebug = flip runReaderT ?logFunc . RIO.logDebug

logInfo :: (HasLogger, MonadIO m) => Utf8Builder -> m ()
logInfo = flip runReaderT ?logFunc . RIO.logInfo

logWarn :: (HasLogger, MonadIO m) => Utf8Builder -> m ()
logWarn = flip runReaderT ?logFunc . RIO.logWarn

logError :: (HasLogger, MonadIO m) => Utf8Builder -> m ()
logError = flip runReaderT ?logFunc . RIO.logError
