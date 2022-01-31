{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module VulkanConfig.FIRUtils where

import Data.Ord (Ordering(LT))
import Data.Type.Ord (OrdCond)
import GHC.TypeLits (KnownSymbol, KnownNat, CmpNat)
import GHC.Records

import FIR
import Math.Linear

instance {-# OVERLAPPING #-}
         (PrimTy a, PrimTyMap r, KnownSymbol x)
        => HasField x (Code (Struct ((x ':-> a) : r))) (Code a) where
  getField = view @(Name x)

instance {-# OVERLAPPABLE #-}
         ( ReifiedGetter (Name x :: Optic '[] (Code (Struct (t:s))) (Code a))
         , HasField x (Code (Struct s)) (Code a))
        => HasField x (Code (Struct (t:s))) (Code a) where
  getField = view @(Name x)

type family SymbolIndex s where
  SymbolIndex "x" = 0
  SymbolIndex "y" = 1
  SymbolIndex "z" = 2
  SymbolIndex "w" = 3
  SymbolIndex "r" = 0
  SymbolIndex "g" = 1
  SymbolIndex "b" = 2
  SymbolIndex "a" = 3
  SymbolIndex "s" = 0
  SymbolIndex "t" = 1
  SymbolIndex "p" = 2
  SymbolIndex "q" = 3

instance (i ~ SymbolIndex s, PrimTy a, KnownNat n, KnownNat i, CmpNat i n ~ LT)
         => HasField s (Code (V n a)) (Code a) where
  getField = view @(Index i)

class SetField x r a | x r -> a where
  setField :: r -> a -> r

instance {-# OVERLAPPING #-}
         (PrimTy a, PrimTyMap r, KnownSymbol x)
        => SetField x (Code (Struct ((x ':-> a) : r))) (Code a) where
  setField = flip $ set @(Name x)

instance {-# OVERLAPPABLE #-}
         ( ReifiedSetter (Name x :: Optic '[] (Code (Struct (t:s))) (Code a))
         , SetField x (Code (Struct s)) (Code a))
        => SetField x (Code (Struct (t:s))) (Code a) where
  setField = flip $ set @(Name x)

instance (i ~ SymbolIndex s, PrimTy a, KnownNat n, KnownNat i, CmpNat i n ~ LT,
          OrdCond (CmpNat 1 n) True True False ~ True)
         => SetField s (Code (V n a)) (Code a) where
  setField = flip $ set @(Index i)
