{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-orphans #-} -- XXX
{-# LANGUAGE StandaloneKindSignatures
           , DataKinds
           , GADTs
           , TypeOperators
           , StandaloneDeriving
           , TypeFamilies
           , UndecidableInstances
           , FunctionalDependencies
           , PolyKinds
           , FlexibleInstances
           , FlexibleContexts
           , AllowAmbiguousTypes
           , BlockArguments
           , TypeApplications
           , ScopedTypeVariables
           , RankNTypes
           , LambdaCase
           , ConstraintKinds
           , OverloadedLabels -- XXX Don't actually need this here
           , ImpredicativeTypes
#-}

module Has where
import RIO (MonadReader) -- XXX don't need this
import Data.Kind
import Data.Tagged
import Data.Type.Equality
import GHC.TypeLits
import GHC.OverloadedLabels

import Control.Lens hiding (without) -- XXX don't want this as a dependency

-- Requirements
type Reqs :: Type
data Reqs = NoReq | Type :.: Reqs

infixr 5 :.:

-- Capabilities
type Caps :: [Type] -> Type
data Caps caps where
  NoCap  :: Caps '[]
  (:..) :: cap `NotIn` caps => cap -> Caps caps -> Caps (cap:caps)

infixr 5 :..

type NoOverlap :: [Type] -> [Type] -> Constraint
type family NoOverlap ts ts' where
  NoOverlap '[]    ts' = ()
  NoOverlap (t:ts) ts' = (t `NotIn` ts', NoOverlap ts ts')

type NotInWitness :: Type -> [Type] -> Type
data NotInWitness t ts where
  NotInWitnessNil  :: NotInWitness t '[]
  NotInWitnessCons :: Unequal t t' => NotInWitness t ts -> NotInWitness t (t':ts)

type Unequal :: Type -> Type -> Constraint
type family Unequal t t' where
  Unequal (Tagged s t) (Tagged s t') = TypeError (Text "Tag " :<>: ShowType s :<>:
                                                  Text " must only appear once")
  Unequal t t' = Unequal' t (t == t')

type Unequal' :: Type -> Bool -> Constraint
type family Unequal' t r where
  Unequal t True = TypeError (ShowType t :<>: Text " is already a capability")
  Unequal t False = ()

type NotIn :: Type -> [Type] -> Constraint
class t `NotIn` ts where
  notInWitness :: NotInWitness t ts

instance t `NotIn` '[] where
  notInWitness = NotInWitnessNil

instance (Unequal t t', t `NotIn` ts) => t `NotIn` (t':ts) where
  notInWitness = NotInWitnessCons notInWitness

type All :: (Type -> Constraint) -> [Type] -> Constraint
type family All c ts where
  All c '[]    = ()
  All c (t:ts) = (c t, All c ts)

-- XXX TODO write better Show instance
deriving instance All Show caps => Show (Caps caps)

type Has :: k -> Type -> Constraint
type family Has caps env where
  Has NoReq          env = ()
  Has (cap :.: caps) env = (HasCap cap env, Has caps env)
  Has cap            env = HasCap cap env

tailLens :: Lens' (Caps (cap:caps)) (Caps caps)
tailLens = lens (\(_ :.. caps) -> caps) \(cap :.. _) caps' -> cap :.. caps'

type PayloadFor :: forall k . Type -> k
type family PayloadFor t where
  PayloadFor (Tagged s t) = t
  PayloadFor t = t

type HasCap :: Type -> Type -> Constraint
class HasCap cap env where
  the' :: Lens' env (PayloadFor cap)

instance {-# OVERLAPPING #-} HasCap (Tagged s cap) (Tagged s cap) where
  the' = lens unTagged (\_ cap' -> Tagged cap')

instance {-# OVERLAPPABLE #-} PayloadFor cap ~ cap => HasCap cap cap where
  the' = id

instance {-# OVERLAPPING #-} HasCap (Tagged s t) (Caps (Tagged s t:caps)) where
  the' = lens (\(cap :.. _) -> untag cap) \(_ :.. caps) cap' -> Tagged @s cap' :.. caps

instance {-# OVERLAPS #-} PayloadFor cap ~ cap => HasCap cap (Caps (cap:caps)) where
  the' = lens (\(cap :.. _) -> cap) \(_ :.. caps) cap' -> cap' :.. caps

instance {-# OVERLAPPABLE #-} HasCap cap (Caps caps) => HasCap cap (Caps (cap':caps)) where
  the' = tailLens.the' @cap

-- class instead of type family so GHCi displays the types a little more
-- nicely: The superclass gets rid of a constraint
-- type HasTag :: k -> Type -> Type -> Constraint
-- class HasCap (Tagged tag a) env => HasTag tag env a | tag env -> a
-- instance HasTag tag (Tagged tag a) a
-- instance {-# OVERLAPPING #-} HasTag tag (Caps (Tagged tag a:env)) a
-- instance {-# OVERLAPPABLE #-} HasTag tag (Caps env) a
--   => HasTag tag (Caps (cap:env)) a

-- XXX Tradeoff: error messages are better with the this definition,
-- but ghci types are slightly better with the above definition
-- But would have to also change HasCap to get better error messages for
-- non-tagged types also
type HasTag :: k -> Type -> Type -> Constraint
type HasTag tag env a = HasTag' tag env a env

type HasTag' :: k -> Type -> Type -> Type -> Constraint
class HasCap (Tagged tag a) env => HasTag' tag env a env' | tag env -> a
instance HasTag' tag (Tagged tag a) a env'
instance {-# OVERLAPPING #-} HasTag' tag (Caps (Tagged tag a:env)) a env'
instance {-# OVERLAPS #-} HasTag' tag (Caps env) a env'
  => HasTag' tag (Caps (cap:env)) a env'
instance {-# OVERLAPPABLE #-}
         -- ( (proxy "Can't find tag", tag, proxy "in", env') ~~ (
         ( TypeError (Text "Can't find tag " :<>: ShowType tag :<>:
                      Text " in " :<>: ShowType env')
         , HasCap (Tagged tag a) env
         , HasTag' tag env a env')
      => HasTag' tag env a env'

class Lookup k t env | env k -> t where
  the :: Lens' env t

instance {-# OVERLAPPING #-} (PayloadFor cap ~ cap, HasCap cap caps)
      => Lookup cap cap caps where
  the = the' @cap

instance {-# OVERLAPPABLE #-} HasTag s caps a
      => Lookup s a caps where
  the = the' @(Tagged s a)

class HasType a env where
  it :: Lens' env a

instance (PayloadFor a ~ a, HasCap a env) => HasType a env where
  it = the' @a

-- XXX TODO: orphan instance, so put this into a separate non-imported module
instance ( lens ~ ((a -> f a) -> env -> f env), Lookup tag a env
         , HasCap (Tagged tag a) env, Functor f)
         => IsLabel tag lens where
  fromLabel = (the' @(Tagged tag a) @env :: Functor f => (a -> f a) -> env -> f env)

type (.) :: k -> Type -> Constraint
type family env . t where
  env . Tagged tag a = HasTag tag env a
  env . a = HasCap a env

infixr 4 .

type (...) :: k -> [Type] -> Constraint
type family env ... ts where
  env ... '[] = ()
  env ... (t:ts) = (env . t, env ... ts)

infixr 4 ...

type (:::) = Tagged

infixr 5 :::

-- XXX example
bar :: (env ... ["foo" ::: (Bool -> Bool), "bar" ::: Bool, String], MonadReader env m)
    => m (String, String, Bool)
bar = do
  f <- view $ the @"foo"
  x <- view #bar -- XXX
  str <- view $ the @String
  str' <- view it
  pure (str, str', f x)
baz :: (String, String, Bool)
baz = bar ('c' >< Tagged @"foo" not >< "hi" >< Tagged @"baz" False >< Tagged @"bar" True)
baz' :: (String, String, Bool)
baz' = bar $ (Tagged @"foo" not >< "hi" >< Tagged @"baz" False >< (5 :: Int) >< Tagged @"bar" True) & the @String <>~ "ho" & the @Int +~ 1

type (++) :: [a] -> [a] -> [a]
type family (++) xs ys where
  '[]    ++ ys = ys
  (x:xs) ++ ys = x : (xs ++ ys)

lemma_notInConcat :: NotInWitness cap caps -> NotInWitness cap caps'
                  -> NotInWitness cap (caps ++ caps')
lemma_notInConcat = \case
  NotInWitnessNil    -> id
  NotInWitnessCons w -> NotInWitnessCons . lemma_notInConcat w

lemma_notInOverlap :: forall cap caps caps' . NoOverlap (cap:caps) caps'
                   => NotInWitness cap caps -> NotInWitness cap (caps ++ caps')
lemma_notInOverlap w = lemma_notInConcat w (notInWitness @cap @caps')

withNotInWitness :: NotInWitness cap caps -> (cap `NotIn` caps => a) -> a
withNotInWitness w x = case w of
  NotInWitnessNil     -> x
  NotInWitnessCons w' -> withNotInWitness w' x

(+++) :: NoOverlap caps caps' => Caps caps -> Caps caps' -> Caps (caps ++ caps')
NoCap +++ caps' = caps'
((cap :: cap) :.. (caps :: Caps caps)) +++ (caps' :: Caps caps') =
  withNotInWitness
    (lemma_notInOverlap @cap @caps @caps' notInWitness)
    (cap :.. (caps +++ caps'))

infixr 5 +++

type Without :: Type -> Type -> Type
type family Without cap caps where
  Without cap caps = Without' cap caps caps

type Without' :: Type -> Type -> Type -> Type
type family Without' cap caps env where
  Without' cap cap env = Caps '[]
  Without' cap (Caps (cap:caps)) env = Caps caps
  Without' cap (Caps (cap':caps)) env = Without'' cap' (Without' cap (Caps caps) env)
  Without' cap caps env =
    TypeError (Text "Capability " :<>: ShowType cap :<>:
               Text " not found in environment " :<>: ShowType env)

type Without'' :: Type -> Type -> Type
type family Without'' cap caps where
  Without'' cap (Caps caps) = Caps (cap : caps)

type In :: Type -> Type -> Constraint
class cap `In` caps where
  without :: caps -> Without cap caps

instance cap `In` cap where
  without _ = NoCap

instance {-# OVERLAPPING #-} Without cap (Caps (cap:caps)) ~ Caps caps
         => cap `In` Caps (cap:caps) where
  without (_ :.. caps) = caps

instance {-# OVERLAPPABLE #-}
         ( Without' cap (Caps (cap':caps)) env ~ Without'' cap' (Without' cap (Caps caps) env)
         , cap `In` Caps caps, Without cap (Caps (cap':caps)) ~ Caps (cap':caps')
         , Without cap (Caps caps) ~ Caps caps', cap' `NotIn` caps')
      => cap `In` (Caps (cap':caps)) where
  without (cap' :.. caps) = cap' :.. without @cap caps

type Concat :: Type -> Type -> Type
type family Concat a b where
  Concat (Caps caps) (Caps caps') = Caps (caps ++ caps')
  Concat (Caps caps) cap' = Caps (caps ++ '[cap'])
  Concat cap (Caps caps') = Caps (cap : caps')
  Concat cap cap' = Caps [cap, cap']

class Catable a b where
  (><) :: a -> b -> Concat a b

instance {-# OVERLAPPING #-} NoOverlap caps caps' => Catable (Caps caps) (Caps caps') where
  (><) = (+++)

instance {-# OVERLAPS #-} ( Concat (Caps caps) cap' ~ Caps (caps ++ '[cap'])
                          , NoOverlap caps '[cap'])
  => Catable (Caps caps) cap' where
  caps >< cap = caps +++ (cap :.. NoCap)

instance {-# OVERLAPS #-} (Concat cap (Caps caps) ~ Caps (cap : caps), cap `NotIn` caps)
  => Catable cap (Caps caps) where
  cap >< caps = cap :.. caps

instance {-# OVERLAPPABLE #-} (Concat cap cap' ~ Caps [cap, cap'], Unequal cap cap')
  => Catable cap cap' where
  cap >< cap' = cap :.. cap' :.. NoCap

with :: Catable caps caps' => caps -> caps' -> Concat caps caps'
with = (><)
