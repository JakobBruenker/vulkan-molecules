{-# LANGUAGE MagicHash #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Utils where

import RIO

import GHC.TypeNats (KnownNat, natVal')
import GHC.Exts (proxy#, Proxy#)
import Foreign.Storable (sizeOf)

import VulkanSetup.Types

logDebug :: HasCallStack => (HasLogger, MonadIO m) => Utf8Builder -> m ()
logDebug = flip runReaderT ?logFunc . RIO.logDebug

logDebugS :: HasCallStack => (HasLogger, MonadIO m) => LogSource -> Utf8Builder -> m ()
logDebugS s = flip runReaderT ?logFunc . RIO.logDebugS s

logInfo :: HasCallStack => (HasLogger, MonadIO m) => Utf8Builder -> m ()
logInfo = flip runReaderT ?logFunc . RIO.logInfo

logInfoS :: HasCallStack => (HasLogger, MonadIO m) => LogSource -> Utf8Builder -> m ()
logInfoS s = flip runReaderT ?logFunc . RIO.logInfoS s

logWarn :: HasCallStack => (HasLogger, MonadIO m) => Utf8Builder -> m ()
logWarn = flip runReaderT ?logFunc . RIO.logWarn

logWarnS :: HasCallStack => (HasLogger, MonadIO m) => LogSource -> Utf8Builder -> m ()
logWarnS s = flip runReaderT ?logFunc . RIO.logInfoS s

logError :: HasCallStack => (HasLogger, MonadIO m) => Utf8Builder -> m ()
logError = flip runReaderT ?logFunc . RIO.logError

logErrorS :: HasCallStack => (HasLogger, MonadIO m) => LogSource -> Utf8Builder -> m ()
logErrorS s = flip runReaderT ?logFunc . RIO.logInfoS s

integralNatVal :: forall n a . (KnownNat n, Integral a) => a
integralNatVal = fromIntegral $ natVal' (proxy# @n)

sizeOfProxied :: forall a . Storable a => Proxy# a -> Int
sizeOfProxied _ = sizeOf (error "evaluated sizeOf argument" :: a)
