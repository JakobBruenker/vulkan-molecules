{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module VulkanConfig.Shaders.ADiff where

import RIO
import RIO.List (unzip)
import RIO.List.Partial (foldr1)
import RIO.Map qualified as M
import Data.Bimap (Bimap)
import Data.Bimap qualified as BM
import RIO.NonEmpty qualified as NE

import Data.Coerce (coerce)
import Data.Monoid qualified as Monoid
import Control.Monad.Extra (fromMaybeM)
import Data.List (find, intercalate, sort)
import FIR qualified
import VulkanConfig.FIRUtils ()
import Data.Tuple.Extra (fst3, thd3, snd3)
import Text.Parsec qualified as P
import Text.Parsec.Expr
import Text.Parsec.Token qualified as P
import Text.Parsec.Language (haskellDef)

import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.Syntax (unsafeTExpCoerce)
import Data.Type.Equality

import Math.Linear qualified as L

import Data.Kind qualified as K

-- a couple of notes:
-- 1. We actually need Vec2, too
-- 2. Common subexpression elimination doesn't work if we have multiple different expressions. So we might need to think of something,
--    e.g. allow functions that keep track of their derivative, maybe in a Map so it's memoized

data Dim = Scalar | Vector2

type SDim :: Dim -> K.Type
data SDim d where
  SScalar :: SDim Scalar
  SVector2 :: SDim Vector2

type KnownDim :: Dim -> K.Constraint
class KnownDim d where
  sDim :: SDim d

instance KnownDim Scalar where
  sDim = SScalar

instance KnownDim Vector2 where
  sDim = SVector2

type Expr :: Dim -> K.Type -> K.Type
data Expr d a where
  (:+) :: Expr d a -> Expr d a -> Expr d a
  (:*) :: KnownDim d => Expr Scalar a -> Expr d a -> Expr d a
  (:**) :: Expr Scalar a -> Expr Scalar a -> Expr Scalar a
  Log :: Expr Scalar a -> Expr Scalar a
  E :: Expr Scalar a -- Euler's constant, kept separate from C for simplification purposes
  C :: a -> Expr Scalar a
  Var :: String -> Expr d a
  Prj1 :: Expr Vector2 a -> Expr Scalar a
  Prj2 :: Expr Vector2 a -> Expr Scalar a
  Vec2 :: Expr Scalar a -> Expr Scalar a -> Expr Vector2 a

deriving instance (Eq a) => Eq (Expr d a)
deriving instance (Ord a) => Ord (Expr d a)

pattern (:-) :: (Num a, Eq a, KnownDim d) => Expr d a -> Expr d a -> Expr d a
pattern e1 :- e2 = e1 :+ C -1 :* e2

pattern (:/) :: (Num a, Eq a, KnownDim d) => Expr d a -> Expr Scalar a -> Expr d a
pattern e1 :/ e2 = e2 :** C -1 :* e1

infixl 6 :+, :-
infixl 7 :*, :/
infixr 8 :**

-- XXX JB maybe we want to make it so this can also keep track of projection of variables (could have a custom var type that can be a projection, and keep track of dim)
toMonomial :: Num a => Expr d a -> Maybe (a, String, Maybe (d :~: Scalar, Expr d a))
toMonomial = \case
  C a :* Var v -> Just (a, v, Nothing)
  C a :* Var v :** b -> Just (a, v, Just (Refl, b))
  Var v :** b -> Just (1, v, Just (Refl, b))
  Var v -> Just (1, v, Nothing)
  _ -> Nothing

pattern Monomial :: forall d a . (KnownDim d, Num a, Eq a) => a -> String -> Maybe (d :~: Scalar, Expr d a) -> Expr d a
pattern Monomial a v p <- (toMonomial -> Just (a, v, p))
  where
    Monomial = \cases
      0 _ (_               ) -> constant 0
      1 v (Nothing         ) ->        Var v
      1 v (Just (Refl, C 1)) ->        Var v
      1 v (Just (Refl, b  )) ->        Var v :** b
      a v (Nothing         ) -> C a :* Var v
      a v (Just (Refl, C 1)) -> C a :* Var v
      a v (Just (Refl, b  )) -> C a :* Var v :** b

toSum :: Expr d a -> Maybe [Expr d a]
toSum = \case
  e1 :+ e2 -> Just $ liftA2 fromMaybe pure toSum =<< [e1, e2]
  _ -> Nothing

pattern Sum :: forall d a . (KnownDim d, Num a) => [Expr d a] -> Expr d a
pattern Sum es <- (toSum -> Just es)
  where
    Sum = maybe (constant 0) (foldr1 (:+)) . NE.nonEmpty

type LastFactor :: Dim -> K.Type -> K.Type
type family LastFactor d a where
  LastFactor Scalar a = ()
  LastFactor Vector2 a = Expr Vector2 a

toProduct :: forall d a . KnownDim d => Expr d a -> Maybe ([Expr Scalar a], LastFactor d a)
toProduct = \case
  e1 :* e2 -> let e1' = fromMaybe [e1] (fst <$> toProduct e1)
                  (e2', last) = case sDim @d of
                    SScalar -> (fromMaybe [e2] (fst <$> toProduct e2), ())
                    SVector2 -> fromMaybe ([], e2) (toProduct e2)
              in Just $ (e1' ++ e2', last)
  _ -> Nothing

pattern Product :: forall d a . (KnownDim d, Num a) => [Expr Scalar a] -> LastFactor d a -> Expr d a
pattern Product es last <- (toProduct -> Just (es, last))
  where
    Product (NE.nonEmpty -> es) last = case sDim @d of
      SScalar -> maybe (C 1) (foldr1 (:*)) es
      SVector2 -> maybe last ((:* last) . (foldr1 (:*))) es

pattern Neg :: (Eq a, Num a, KnownDim d) => Expr d a -> Expr d a
pattern Neg e = C -1 :* e

pattern Exp :: Eq a => d ~ Scalar => Expr d a -> Expr d a
pattern Exp e = E :** e

pattern Sqrt :: (Eq a, Floating a, KnownDim d) => d ~ Scalar => Expr d a -> Expr d a
pattern Sqrt e = e :** C 0.5

{-# COMPLETE C, Var, (:**), E, Log, Sum, Product, Prj1, Prj2, Vec2 #-}
{-# COMPLETE C, Monomial, (:**), E, Log, Sum, Product, Prj1, Prj2, Vec2 #-}

instance (KnownDim d, Num a, Eq a, Show a) => Show (Expr d a) where
  show = case sDim @d of
    SScalar -> \case
      C a -> show a
      Var v -> v
      Sum es -> "(" ++ intercalate " + " (show <$> es) ++ ")"
      Product es () -> "(" ++ intercalate " * " (show <$> es) ++ ")"
      Exp e -> "exp " ++ show e
      E -> show $ Exp @a (C 1)
      e1 :** e2 -> "(" ++ show e1 ++ " ** " ++ show e2 ++ ")"
      Log e -> "ln " ++ show e
      Prj1 e -> show e ++ "₁"
      Prj2 e -> show e ++ "₂"
    SVector2 -> \case
      Var v -> v
      Sum es -> "(" ++ intercalate " + " (show <$> es) ++ ")"
      Product es v -> "(" ++ intercalate " * " (show <$> es) ++ " * " ++ show v ++ ")"
      Vec2 a b -> "(" ++ show a ++ ", " ++ show b ++ ")"

instance IsString (Expr d a) where
  fromString = Var

-- We only support differentiating scalars, though they can be differentiated with respect to vectors
diff :: forall d a . (KnownDim d, Num a, Eq a) => String -> Expr Scalar a -> Expr d a
diff v = case (sDim @d) of
  _ -> \case
    C _ -> constant 0
    Var v' -> constant if v == v' then 1 else 0
    e1 :+ e2 -> diff v e1 :+ diff v e2
    e1 :* e2 -> case sDim @d of
      _ -> e1 :* diff v e2 :+ e2 :* diff v e1
    Exp e -> Exp e :* diff v e
    E -> constant 0
    e1 :** e2 -> e1 :** e2 :* (Log e1 :* diff v e2 :+ e2 :* diff v e1 :/ e1)
    Log e -> diff v e :/ e
    Prj1 (prj1 -> e) | Prj1 (Var v') <- e -> case sDim @d of SScalar -> constant 0 -- v' and v have to be different variables
                                                             SVector2 -> if v == v' then Vec2 (C 1) (C 0) else constant 0
                     | otherwise -> diff v e
    Prj2 (prj2 -> e) | Prj2 (Var v') <- e -> case sDim @d of SScalar -> constant 0 -- v' and v have to be different variables
                                                             SVector2 -> if v == v' then Vec2 (C 0) (C 1) else constant 0
                     | otherwise -> diff v e

prj1 :: Expr Vector2 a -> Expr Scalar a
prj1 = \case
  u :+ v -> prj1 u :+ prj1 v
  u :* v -> u :* prj1 v
  Vec2 a _ -> a
  Var v -> Prj1 (Var v)

prj2 :: Expr Vector2 a -> Expr Scalar a
prj2 = \case
  u :+ v -> prj2 u :+ prj2 v
  u :* v -> u :* prj2 v
  Vec2 _ b -> b
  Var v -> Prj2 (Var v)

type DimConst :: Dim -> K.Type -> K.Type
type family DimConst d a where
  DimConst Scalar a = a
  DimConst Vector2 a = (a, a)

partitionExprs :: (KnownDim d, Num a, Eq a) => [Expr d a] -> ([a], [(a, String, Maybe (d :~: Scalar, Expr d a))], [Expr d a], Maybe (d :~: Vector2, [(Expr Scalar a, Expr Scalar a)]))
partitionExprs = foldr (flip \(cs, ms, es, vs) -> \case
    C a -> (a:cs, ms, es, vs)
    Vec2 a b -> (cs, ms, es, case vs of
      Nothing -> Just (Refl, [(a, b)])
      Just vs' -> Just $ ((a, b):) <$> vs')
    Monomial a v p -> (cs, (a, v, p):ms, es, vs)
    e -> (cs, ms, e:es, vs)
  ) ([], [], [], Nothing)

constant :: forall d a . (KnownDim d) => a -> Expr d a
constant (C -> x) = case sDim @d of
  SScalar -> x
  SVector2 -> Vec2 x x

repeatUntilFixpoint :: Eq a => (a -> a) -> a -> a
repeatUntilFixpoint f x = let x' = f x in if x == x' then x else repeatUntilFixpoint f x'

simplify :: forall a d . (KnownDim d, FIR.AdditiveGroup a, Ord a, Floating a)
         => Expr d a -> Expr d a
simplify = repeatUntilFixpoint step
  where
    step :: forall d' . (KnownDim d') => Expr d' a -> Expr d' a
    step = \case
      C a -> C a

      Monomial a v p -> Monomial a v (fmap step <$> p)

      Sum es -> Sum $ concat [c', ms', sort $ map step es']
        where
          (cs, ms, es', vs) = partitionExprs es
          ms' = map (\mons@((_, v, p) :| _) -> Monomial (sum $ fmap fst3 mons) v (fmap step <$> p)) . NE.groupWith (\(_, v, p) -> (v, p)) . sort $ ms
          c' :: [Expr d' a]
          c' = case sDim @d' of
            SScalar | s <- C (L.sum cs) -> [s | s /= constant 0]
            SVector2 -> maybe [] pure $ find (/= Vec2 (C 0) (C 0)) vs'
          vs' = uncurry Vec2 . join bimap (step . Sum) . unzip . snd <$> vs

      a :* Vec2 b (C 0) -> Vec2 (step (a :* b)) (C 0)
      a :* Vec2 (C 0) b -> Vec2 (C 0) (step (a :* b))
      Product es last -> if isZero then constant 0 else Product (concat [[C c' | c' /= 1], ms', sort $ map step es']) last'
        where
          (isZero, last') = case sDim @d' of
            SScalar -> (c' == 0, ())
            SVector2 -> (last == Vec2 (C 0) (C 0), step last)
          (cs, ms, es', _impossible) = partitionExprs es
          ms' = map (\mons@((_, v, _) :| _) -> Monomial (product $ fmap fst3 mons) v (Just (Refl, Sum . sort . toList $ fmap (maybe (C 1) (step . snd) . thd3) mons))) .
                NE.groupWith snd3 .
                sort $ ms
          c' = product cs

      Exp (C a) -> C $ exp a
      Exp (Log a) -> step a
      Exp a -> Exp (step a)

      E -> E

      C a :** C b -> C $ a ** b
      _ :** C 0 -> C 1
      a :** C 1 -> step a
      (a :** b) :** c -> step a :** (step b :* step c)
      a :** b -> step a :** step b

      Log (C 1) -> C 0
      Log (C a) -> C $ log a
      Log E -> C 1
      Log (Exp a) -> step a
      Log (e@(_ :* _)) -> case simplify e of
        a :* b -> Log a :+ Log b
        e' -> Log e'
      Log (a :** b) -> step b :* Log (step a)
      Log a -> Log (step a)

      Prj1 (Vec2 a _) -> step a
      Prj1 a -> Prj1 (step a)

      Prj2 (Vec2 _ b) -> step b
      Prj2 a -> Prj2 (step a)

      Vec2 (extractConst -> Just (a, a')) (extractConst -> Just (b, b')) | a == b -> C a :* Vec2 a' b'
      Vec2 a b -> Vec2 (step a) (step b)
      where
        extractConst :: Expr Scalar a -> Maybe (a, Expr Scalar a)
        extractConst = \case
          C a -> Just (a, C 1)
          C a :* a' -> Just (a, a')
          _ -> Nothing

-- XXX JB obviously this should already get the stringified Map
substitute :: forall d a . (KnownDim d, Ord a, Num a, Show a) => Bimap (Expr Vector2 a) String -> Bimap (Expr Scalar a) String -> Expr d a -> Expr d a
substitute vs ss = \case
  C a -> C a
  Var v -> fromMaybe (Var v) $ BM.lookupR v case sDim @d of
    SScalar -> ss
    SVector2 -> vs
  E -> E
  Log a -> Log (substitute vs ss a)
  a :+ b -> substitute vs ss a :+ substitute vs ss b
  a :* b -> substitute vs ss a :* substitute vs ss b
  a :** b -> substitute vs ss a :** substitute vs ss b
  Prj1 v -> Prj1 (substitute vs ss v)
  Prj2 v -> Prj2 (substitute vs ss v)
  Vec2 a b -> Vec2 (substitute vs ss a) (substitute vs ss b)

-- XXX JB plan:
-- - simplify
-- - extract sub exprs
-- - simplify both expr and sub exprs (including extracting sub exprs etc.)
-- - substitute
-- - repeat until nothing changes

createBimap :: (Ord a, KnownDim d, Num a, Show a) => [Expr d a] -> Bimap (Expr d a) String
createBimap = BM.fromList . map (id &&& show)


-- first try topmost
-- keep track of how many substs there are
-- if any gets replace as many times as it exists, remove it from list
-- simplify - if something changes, start over
-- otherwise, substitute, and then replace again with the rest, until nothing is left in the map
extractSubExprs :: forall d a . (KnownDim d, Ord a, Num a, Show a) => Expr d a -> (Map (Expr Vector2 a) Natural, Map (Expr Scalar a) Natural, Expr d a)
extractSubExprs ex = (vectorCounts, scalarCounts, replaceTopMost ex)
  where
    -- simplifyBySubExpr :: forall d' . KnownDim d' => Map (Expr Vector2 a) Natural -> Map (Expr Scalar a) Natural -> Expr d' a -> Expr d' a
    -- simplifyBySubExpr vs ss e
    --   | M.null vs, M.null ss = e
    --   | otherwise = 

    replaceTopMost :: forall d' . KnownDim d' => Expr d' a -> Expr d' a
    replaceTopMost e = flip fromMaybe (Var <$> replace e) case e of
      C a -> C a
      Var v -> Var v
      E -> E
      Log a -> Log (replaceTopMost a)
      e1 :+ e2 -> replaceTopMost e1 :+ replaceTopMost e2
      e1 :* e2 -> replaceTopMost e1 :* replaceTopMost e2
      e1 :** e2 -> replaceTopMost e1 :** replaceTopMost e2
      Prj1 v -> Prj1 (replaceTopMost v)
      Prj2 v -> Prj2 (replaceTopMost v)
      Vec2 e1 e2 -> Vec2 (replaceTopMost e1) (replaceTopMost e2)
    
    replace :: forall d' . KnownDim d' => Expr d' a -> Maybe String
    replace e = BM.lookup e strings <* (find @_ @Natural (>= 2) $ M.lookup e counts)
      where
        (counts, strings) = case sDim @d' of
          SScalar -> (filteredScalarCounts, scalarStrings)
          SVector2 -> (filteredVectorCounts, vectorStrings)
    
    vectorCounts :: Map (Expr Vector2 a) Natural
    scalarCounts :: Map (Expr Scalar a) Natural
    (vectorCounts, scalarCounts) = coerce $ exprMap ex

    filteredVectorCounts :: Map (Expr Vector2 a) Natural
    filteredVectorCounts = M.filter (>= 2) vectorCounts

    filteredScalarCounts :: Map (Expr Scalar a) Natural
    filteredScalarCounts = M.filter (>= 2) scalarCounts

    vectorStrings :: BM.Bimap (Expr Vector2 a) String
    vectorStrings = createBimap $ M.keys vectorCounts

    scalarStrings :: BM.Bimap (Expr Scalar a) String
    scalarStrings = createBimap $ M.keys scalarCounts

    exprMap :: forall d' . KnownDim d' => Expr d' a -> (Map (Expr Vector2 a) (Monoid.Sum Natural), Map (Expr Scalar a) (Monoid.Sum Natural))
    exprMap e =
      let current = M.singleton e (Monoid.Sum @Natural 1)
          maps :: (Map (Expr Vector2 a) (Monoid.Sum Natural), Map (Expr Scalar a) (Monoid.Sum Natural))
          maps = case sDim @d' of
            SScalar -> (mempty, current)
            SVector2 -> (current, mempty)
      in case e of
        C _ -> (mempty, mempty)
        Var _ -> (mempty, mempty)
        E -> (mempty, mempty)
        Log e' -> maps <.> exprMap e'
        e1 :+ e2 -> maps <.> exprMap e1 <.> exprMap e2
        e1 :* e2 -> maps <.> exprMap e1 <.> exprMap e2
        e1 :** e2 -> maps <.> exprMap e1 <.> exprMap e2
        Prj1 e' -> maps <.> exprMap e'
        Prj2 e' -> maps <.> exprMap e'
        Vec2 e1 e2 -> maps <.> exprMap e1 <.> exprMap e2
    
    (a, b) <.> (c, d) = (M.unionWith (<>) a c, M.unionWith (<>) b d)

parse :: String -> Either P.ParseError Exp
parse = P.parse (P.spaces *> ex) ""
  where
    ex = buildExpressionParser table term
    term = parens ex <|> appCon 'C <$> number <|> appCon 'Var <$> identifier
    table =
      [ [postfix "₁" 'Prj1, postfix "_1" 'Prj1, postfix "₂" 'Prj2, postfix "_2" 'Prj2]
      , [prefix "ln" 'Log, prefix "exp" 'Exp, prefix "sqrt" 'Sqrt]
      , [binary "**" '(:**) AssocRight]
      , [binary "*" '(:*) AssocLeft, binary "/" '(:/) AssocLeft]
      , [binary "+" '(:+) AssocLeft, binary "-" '(:-) AssocLeft, prefix "-" 'Neg]
      , [binary "," 'Vec2 AssocNone]
      ]

    appCon con = AppE (ConE con)

    binary name funName assoc = flip Infix assoc do
      reservedOp name
      pure \a b -> InfixE (Just a) (ConE funName) (Just b)
    prefix name funName = Prefix do
      reservedOp name
      pure (appCon funName)
    postfix name funName = Postfix do
      reservedOp name
      pure (appCon funName)

    lexer = P.makeTokenParser haskellDef
      { P.reservedOpNames = ["+", "-", "*", "/", "**"]
      , P.reservedNames = ["ln"]
      }

    parens = P.parens lexer
    number = LitE . either IntegerL (RationalL . toRational) <$> P.naturalOrFloat lexer
    identifier = LitE . StringL <$> P.identifier lexer
    reservedOp = P.reservedOp lexer

expr :: QuasiQuoter
expr = QuasiQuoter
  { quoteExp = either (error . show) pure . parse
  , quotePat = error "expr must be used as an expression, not a pattern"
  , quoteType = error "expr must be used as an expression, not a type"
  , quoteDec = error "expr must be used as an expression, not a declaration"
  }

type DimToFIR :: Dim -> K.Type -> K.Type
type family DimToFIR d a where
  DimToFIR Scalar a = FIR.Code a
  DimToFIR Vector2 a = FIR.Code (L.V 2 a)

toCode :: forall d . KnownDim d => Expr d Float -> Code Q (DimToFIR d Float)
toCode = case sDim @d of
  SScalar -> \case
    C a -> [|| FIR.Lit a ||]
    Var v -> liftCode do
      name <- fromMaybeM (fail $ "Couldn't find variable " ++ v) (lookupValueName v)
      ty <- reifyType name
      when (ty /= ConT ''FIR.Code `AppT` ConT ''Float) do
        fail $ "Variable " ++ v ++ " has type " ++ show ty ++ ", must be Code Float"
      unsafeTExpCoerce (varE name)
    a :- b -> [|| $$(toCode a) FIR.- $$(toCode b) ||]
    a :+ b -> [|| $$(toCode a) FIR.+ $$(toCode b) ||]
    a :/ b -> [|| $$(toCode a) FIR./ $$(toCode b) ||]
    a :* b -> [|| $$(toCode a) FIR.* $$(toCode b) ||]
    Exp a -> [|| FIR.exp $$(toCode a) ||]
    E -> [|| FIR.exp (FIR.Lit 1) ||]
    a :** b -> [|| $$(toCode a) FIR.** $$(toCode b) ||]
    Log a -> [|| FIR.log $$(toCode a) ||]
    Prj1 a -> [|| $$(toCode a).x ||]
    Prj2 a -> [|| $$(toCode a).y ||]
  SVector2 -> \case
    Vec2 a b -> [|| FIR.Vec2 $$(toCode a) $$(toCode b) ||]
    Var v -> liftCode do
      name <- fromMaybeM (fail $ "Couldn't find variable " ++ v) (lookupValueName v)
      ty <- reifyType name
      when (ty /= ConT ''FIR.Code `AppT` (ConT ''L.V `AppT` LitT (NumTyLit 2) `AppT` ConT ''Float)) do
        fail $ "Variable " ++ v ++ " has type " ++ show ty ++ ", must be Code (V 2 Float)"
      unsafeTExpCoerce (varE name)
    a :- b -> [|| $$(toCode a) L.^-^ $$(toCode b) ||]
    a :+ b -> [|| $$(toCode a) L.^+^ $$(toCode b) ||]
    a :* b -> [|| $$(toCode a) L.*^  $$(toCode b) ||]

simpleCode :: KnownDim d => Expr d Float -> Code Q (DimToFIR d Float)
simpleCode = toCode . simplify
