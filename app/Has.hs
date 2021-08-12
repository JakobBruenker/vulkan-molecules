{-# OPTIONS_GHC -Wno-redundant-constraints #-}
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
#-}

module Has where

import Data.Kind
import Data.Tagged
import Data.Type.Equality
import GHC.TypeLits

import Lens.Micro
import Lens.Micro.Extras

-- Requirements
type Reqs :: Type
data Reqs = NoReq | Type :.: Reqs

infixr 5 :.:

-- Capabilities
type Caps :: [Type] -> Type
data Caps caps where
  NoCap  :: Caps '[]
  (:::) :: cap `NotIn` caps => cap -> Caps caps -> Caps (cap:caps)

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

deriving instance All Show caps => Show (Caps caps)

type Has :: k -> Type -> Constraint
type family Has caps env where
  Has NoReq          env = ()
  Has (cap :.: caps) env = (HasCap cap env, Has caps env)
  Has cap            env = HasCap cap env

tailLens :: Lens' (Caps (cap:caps)) (Caps caps)
tailLens = lens (\(_ ::: caps) -> caps) \(cap ::: _) caps' -> cap ::: caps'

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
  the' = lens (\(cap ::: _) -> untag cap) \(_ ::: caps) cap' -> Tagged @s cap' ::: caps

instance {-# OVERLAPS #-} PayloadFor cap ~ cap => HasCap cap (Caps (cap:caps)) where
  the' = lens (\(cap ::: _) -> cap) \(_ ::: caps) cap' -> cap' ::: caps

instance {-# OVERLAPPABLE #-} HasCap cap (Caps caps) => HasCap cap (Caps (cap':caps)) where
  the' = tailLens.the' @cap

type EnvLookup :: k -> k' -> Type
type family EnvLookup k env where
  EnvLookup s env = EnvLookup' s env env

type EnvLookup' :: k -> k' -> k' -> Type
type family EnvLookup' k env' env where
  EnvLookup' s env' (Tagged s t) = t
  EnvLookup' s env' (Caps (Tagged s t:env)) = t
  EnvLookup' s env' (Caps (cap:env)) = EnvLookup s (Caps env)
  EnvLookup' s env' _ = TypeError (Text "Could not find tag " :<>: ShowType s :<>:
                                   Text " in the environment " :<>: ShowType env')

-- XXX
-- class Lookup k t env where
class Lookup k t env | env k -> t where
  the :: Lens' env t

instance {-# OVERLAPPING #-} (PayloadFor cap ~ cap, Has cap caps)
  => Lookup cap cap caps where
  the = the' @cap

-- EnvLookup is here to make the fundep work
instance {-# OVERLAPPABLE #-} (EnvLookup s caps ~ t, Has (Tagged s t) caps)
  => Lookup s t caps where
  the = the' @(Tagged s t)

-- XXX Maybe have operators to allow something like
-- env . "foo" : Bool
-- env ... ["foo" : Bool, "bar" : Bool -> Bool]
type HasTag :: Type -> Type -> Constraint
type family HasTag env tagged where
  HasTag env (Tagged tag a) = (EnvLookup tag env ~ a, Has (Tagged tag a) env)

type (.) :: k -> Type -> Constraint
type family env . t where
  env . Tagged tag a = (EnvLookup tag env ~ a, HasCap (Tagged tag a) env)
  env . a = HasCap a env

infixr 4 .

type (...) :: k -> [Type] -> Constraint
type family env ... ts where
  env ... '[] = ()
  env ... (t:ts) = (env . t, env ... ts)

infixr 4 ...

type (:::) = Tagged

infixr 5 :::

bar :: env ... ["foo" ::: (Bool -> Bool), "bar" ::: Bool, String]
    => env -> (String, Bool)
bar = do
  f <- view $ the @"foo"
  x <- view $ the @"bar"
  str <- view $ the @String
  pure (str, f x)
baz :: (String, Bool)
baz = bar (Tagged @"foo" not ::: "hi" ::: Tagged @"bar" True ::: NoCap)

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
((cap :: cap) ::: (caps :: Caps caps)) +++ (caps' :: Caps caps') =
  withNotInWitness
    (lemma_notInOverlap @cap @caps @caps' notInWitness)
    (cap ::: (caps +++ caps'))

infixr 5 +++

asCap :: t -> Caps '[t]
asCap = (::: NoCap)

-- TODO: remove element from list
-- without :: forall cap caps . cap `In` caps => Caps caps -> Caps (Without cap caps)

-- class Catable a b where
--   type Concat a b
--   (><) :: a -> b -> Concat a b

-- instance Catable (Caps caps) (Caps caps') where
--   type Concat (Caps caps) (Caps caps') = Caps (caps ++ caps')
--   (><) = (+++)

-- instance Catable cap (Caps caps) where
--   type Concat cap (Caps caps) = Caps (cap:caps)
