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

logInfo :: HasCallStack => (HasLogger, MonadIO m) => Utf8Builder -> m ()
logInfo = flip runReaderT ?logFunc . RIO.logInfo

logWarn :: HasCallStack => (HasLogger, MonadIO m) => Utf8Builder -> m ()
logWarn = flip runReaderT ?logFunc . RIO.logWarn

logError :: HasCallStack => (HasLogger, MonadIO m) => Utf8Builder -> m ()
logError = flip runReaderT ?logFunc . RIO.logError

integralNatVal :: forall n a . (KnownNat n, Integral a) => a
integralNatVal = fromIntegral $ natVal' (proxy# @n)

sizeOfProxied :: forall a . Storable a => Proxy# a -> Int
sizeOfProxied _ = sizeOf (error "evaluated sizeOf argument" :: a)
