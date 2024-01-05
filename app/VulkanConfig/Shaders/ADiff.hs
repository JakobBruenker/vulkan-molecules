{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module VulkanConfig.Shaders.ADiff where

import RIO
import RIO.List (iterate)
import RIO.List.Partial (tail, foldr1)
import RIO.NonEmpty qualified as NE

import Data.List (find, intercalate, sort)
import FIR qualified
import Data.Maybe (fromJust)
import Data.Tuple.Extra (fst3, thd3, snd3)
import Text.Parsec qualified as P
import Text.Parsec.Expr
import Text.Parsec.Token qualified as P
import Text.Parsec.Language (haskellDef)

import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH

data Expr a where
  (:+), (:*), (:^) :: Expr a -> Expr a -> Expr a
  Ln :: Expr a -> Expr a
  C :: a -> Expr a
  Var :: String -> Expr a
  deriving Eq

pattern (:-) :: (Eq a, DiffInt a) => Expr a -> Expr a -> Expr a
pattern e1 :- e2 <- e1 :+ C ((== negOne) -> True) :* e2
  where
    e1 :- e2 = e1 :+ C negOne :* e2

pattern (:/) :: (Eq a, DiffInt a) => Expr a -> Expr a -> Expr a
pattern e1 :/ e2 <- e1 :* e2 :^ C ((== negOne) -> True)
  where
    e1 :/ e2 = e1 :* e2 :^ C negOne

infixl 6 :+, :-
infixl 7 :*, :/
infixr 8 :^

toMonomial :: DiffInt a => Expr a -> Maybe (a, String, Expr a)
toMonomial = \case
  C a :* Var v -> Just (a, v, C one)
  C a :* Var v :^ b -> Just (a, v, b)
  Var v :^ b -> Just (one, v, b)
  Var v -> Just (one, v, C one)
  _ -> Nothing

pattern Monomial :: (Eq a, DiffInt a) => (a, String, Expr a) -> Expr a
pattern Monomial e <- (toMonomial -> Just e)
  where
    Monomial = \case
      (Zero, _, _    ) -> C zero
      (One , v, C One) ->        Var v
      (One , v, b    ) ->        Var v :^ b
      (a   , v, C One) -> C a :* Var v
      (a   , v, b    ) -> C a :* Var v :^ b

toSum :: Expr a -> Maybe [Expr a]
toSum = \case
  e1 :+ e2 -> Just $ liftA2 fromMaybe pure toSum =<< [e1, e2]
  _ -> Nothing

pattern Sum :: DiffInt a => [Expr a] -> Expr a
pattern Sum es <- (toSum -> Just es)
  where
    Sum = maybe (C zero) (foldr1 (:+)) . NE.nonEmpty

toProduct :: Expr a -> Maybe [Expr a]
toProduct = \case
  e1 :* e2 -> Just $ liftA2 fromMaybe pure toProduct =<< [e1, e2]
  _ -> Nothing

pattern Product :: DiffInt a => [Expr a] -> Expr a
pattern Product es <- (toProduct -> Just es)
  where
    Product = maybe (C one) (foldr1 (:*)) . NE.nonEmpty

pattern Neg :: (DiffInt a, Eq a) => Expr a -> Expr a
pattern Neg e = C NegOne :* e

{-# COMPLETE C, Var, (:^), Ln, Sum, Product #-}
{-# COMPLETE C, Monomial, (:^), Ln, Sum, Product #-}

instance (Ord a, DiffInt a) => Ord (Expr a) where
  compare = \cases
    (C a1) (C a2) -> a1 `compare` a2
    (C _) _ -> LT
    (Monomial (a1, v1, p1)) (Monomial (a2, v2, p2)) -> case compare v1 v2 of
      LT -> LT
      EQ -> case compare p1 p2 of
        LT -> LT
        EQ -> a1 `compare` a2
        GT -> GT
      GT -> GT
    (Monomial _) _ -> LT
    (e1 :^ e2) (e3 :^ e4) -> case compare e1 e3 of
      LT -> LT
      EQ -> e2 `compare` e4
      GT -> GT
    (_ :^ _) _ -> LT
    (Ln e1) (Ln e2) -> e1 `compare` e2
    (Ln _) _ -> LT
    (Sum es1) (Sum es2) -> case compare (length es1) (length es2) of
      LT -> LT
      EQ -> compare es1 es2
      GT -> GT
    (Sum _) _ -> LT
    (Product es1) (Product es2) -> case compare (length es1) (length es2) of
      LT -> LT
      EQ -> compare es1 es2
      GT -> GT
    (Product _) _ -> LT

instance (DiffInt a, Show a) => Show (Expr a) where
  show = \case
    C a -> show a
    Var v -> v
    Sum es -> "(" ++ intercalate " + " (show <$> es) ++ ")"
    Product es -> "(" ++ intercalate " * " (show <$> es) ++ ")"
    e1 :^ e2 -> "(" ++ show e1 ++ " ^ " ++ show e2 ++ ")"
    Ln e -> "ln " ++ show e

instance IsString (Expr a) where
  fromString = Var

class DiffInt a where
  negOne, zero, one, two :: a

instance {-# OVERLAPPABLE #-} Num a => DiffInt a where
  negOne = -1
  zero = 0
  one = 1
  two = 2

instance {-# OVERLAPPING #-} (FIR.PrimTy a, DiffInt a) => DiffInt (FIR.Code a) where
  negOne = FIR.Lit negOne
  zero = FIR.Lit zero
  one = FIR.Lit one
  two = FIR.Lit two

pattern NegOne :: (Eq a, DiffInt a) => a
pattern NegOne <- ((== negOne) -> True)
  where
    NegOne = negOne

pattern Zero :: (Eq a, DiffInt a) => a
pattern Zero <- ((== zero) -> True)
  where
    Zero = zero

pattern One :: (Eq a, DiffInt a) => a
pattern One <- ((== one) -> True)
  where
    One = one

pattern Two :: (Eq a, DiffInt a) => a
pattern Two <- ((== two) -> True)
  where
    Two = two

diff :: (Eq a, DiffInt a) => String -> Expr a -> Expr a
diff v = \case
  C _ -> C zero
  Var v' -> C $ if v == v' then one else zero
  e1 :+ e2 -> diff v e1 :+ diff v e2
  e1 :* e2 -> e1 :* diff v e2 :+ diff v e1 :* e2
  e1 :^ e2 -> e1 :^ e2 :* (diff v e2 :* Ln e1 :+ e2 :* diff v e1 :/ e1)
  Ln e -> diff v e :/ e

partitionExprs :: (DiffInt a, Eq a) => [Expr a] -> ([a], [(a, String, Expr a)], [Expr a])
partitionExprs = foldr (flip \(cs, ms, es) -> \case
    C a -> (a:cs, ms, es)
    Monomial m -> (cs, m:ms, es)
    e -> (cs, ms, e:es)
  ) ([], [], [])

simplify :: (Floating a, DiffInt a, Ord a) => Expr a -> Expr a
simplify = fst . fromJust . find (uncurry (==)) . (zip <*> tail) . iterate step
  -- simplify until we reach a fixed point
  -- find can never produce `Nothing` here, since it's an infinite list
  where
    step = \case
      C a -> C a

      Monomial (a, v, p) -> Monomial (a, v, p)

      Sum es -> Sum $ concat [[C c' | c' /= zero], ms', sort $ map step es']
        where
          (cs, ms, es') = partitionExprs es
          ms' = map (\mons@((_, v, p) :| _) -> Monomial (sum $ fmap fst3 mons, v, step p)) . NE.groupWith (\(_, v, p) -> (v, p)) . sort $ ms
          c' = sum cs

      Product es -> if c' == zero then C zero else Product $ concat [[C c' | c' /= one], ms', sort $ map step es']
        where
          (cs, ms, es') = partitionExprs es
          ms' = map (\mons@((_, v, _) :| _) -> Monomial (product $ fmap fst3 mons, v, Sum . sort . toList $ fmap (step . thd3) mons)) . NE.groupWith snd3 . sort $ ms
          c' = product cs

      C a :^ C b -> C $ a ** b
      _ :^ C Zero -> C one
      a :^ C One -> step a
      (a :^ b) :^ c -> step a :^ (step b :* step c)
      a :^ b -> step a :^ step b

      Ln (C One) -> C zero
      Ln (C a) -> C $ log a
      Ln (a :^ b) -> step b :* Ln (step a)
      Ln a -> Ln (step a)

parse :: String -> Either P.ParseError Exp
parse = P.parse ex ""
  where
    ex = buildExpressionParser table term
    term = parens ex <|> appCon 'C <$> number <|> appCon 'Var <$> identifier
    table =
      [ [prefix "ln" 'Ln]
      , [binary "^" '(:^) AssocRight]
      , [binary "*" '(:*) AssocLeft, binary "/" '(:/) AssocLeft]
      , [binary "+" '(:+) AssocLeft, binary "-" '(:-) AssocLeft, prefix "-" 'Neg]
      ]

    appCon con = AppE (ConE con)

    binary name funName assoc = flip Infix assoc do
      reservedOp name
      pure \a b -> InfixE (Just a) (ConE funName) (Just b)
    prefix name funName = Prefix do
      reservedOp name
      pure (appCon funName)

    lexer = P.makeTokenParser haskellDef
      { P.reservedOpNames = ["+", "-", "*", "/", "^"]
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

-- TODO:
-- - interface with TH
-- - common subexpression elimination
-- - Plan:
--   1. QQ to parse expression: diff "x" [expr| x^2 + y |]
--   2. differentiate and simplify, grouping constants
--   3. when generating FIR code, (precompute any constants)
