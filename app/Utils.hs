module Utils where

import RIO
import Data.Tuple (swap)
import Control.Monad.Trans.Cont (ContT (ContT))
import Vulkan (Result (SUCCESS))
import Types

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

-- This is probably unnecessary, since the library says it already throws
-- an exception if the result is not SUCCESS
catchVk :: MonadIO m => m (Result, a) -> m a
catchVk = (=<<) \case
  (SUCCESS, x) -> pure x
  (err    , _) -> throwIO $ VkGenericError err
