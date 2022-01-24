{-# LANGUAGE MagicHash #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Utils where

import RIO

import GHC.TypeNats(KnownNat, natVal')
import GHC.Exts(proxy#)

import Graphics.Types

logDebug :: HasCallStack => (HasLogger, MonadIO m) => Utf8Builder -> m ()
logDebug = flip runReaderT ?logFunc . RIO.logDebug

logInfo :: HasCallStack => (HasLogger, MonadIO m) => Utf8Builder -> m ()
logInfo = flip runReaderT ?logFunc . RIO.logInfo

logWarn :: HasCallStack => (HasLogger, MonadIO m) => Utf8Builder -> m ()
logWarn = flip runReaderT ?logFunc . RIO.logWarn

logError :: HasCallStack => (HasLogger, MonadIO m) => Utf8Builder -> m ()
logError = flip runReaderT ?logFunc . RIO.logError

integralNatVal :: forall n a . (KnownNat n, Integral a) => a
integralNatVal = fromIntegral $ natVal' (proxy# @n)
