module Utils where

import Control.Applicative (liftA2)
import Data.Tuple (swap)
import Data.Bool (bool)
import Data.String (IsString)
import Control.Monad.Trans.Cont (ContT (ContT))
import RIO (MonadUnliftIO, bracket)

traverseToSnd :: Functor f => (a -> f b) -> a -> f (a, b)
traverseToSnd = liftA2 fmap (,)

traverseToFst :: Functor f => (a -> f b) -> a -> f (b, a)
traverseToFst = (fmap swap .) . traverseToSnd

plural :: (Semigroup a, IsString a, Eq b, Num b) => a -> b -> a
plural str n = str <> bool "s" "" (n == 1)

-- in base since 4.16
clamp :: Ord a => (a, a) -> a -> a
clamp (low, high) a = min high (max a low)

bracketCont :: MonadUnliftIO m => m a -> (a -> m b) -> ContT r m a
bracketCont = (ContT .) . bracket
