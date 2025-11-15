{-# LANGUAGE InstanceSigs, BangPatterns #-}
module HaskellUtils.FoldableT where

import Control.Monad

newtype EndoM m a = EndoM { appEndoM :: a -> m a }

instance (Monad m) => Semigroup (EndoM m a) where
  (<>) :: Monad m => EndoM m a -> EndoM m a -> EndoM m a
  EndoM f <> EndoM g = EndoM (f <=< g)

instance (Monad m) => Monoid (EndoM m a) where
  mempty :: Monad m => EndoM m a
  mempty = EndoM pure

class FoldableT t where
  foldMapT :: (Monad m, Monoid n) => (a -> n) -> t m a -> m n
  foldrT :: Monad m => (a -> b -> m b) -> b -> t m a -> m b

  foldrT f z t = foldMapT (EndoM . f) t >>= \endo -> appEndoM endo z
  foldMapT f = foldrT (\a m -> pure (m <> f a)) mempty

  foldlT :: Monad m => (b -> a -> m b) -> b -> t m a -> m b
  foldlT = undefined

  foldlT' :: Monad m => (b -> a -> m b) -> b -> t m a -> m b
  foldlT' f z t = do
    !z' <- foldlT f z t
    pure z'

  foldT :: (Monad m, Monoid n) => t m n -> m n
  foldT = foldMapT id

  toListT :: Monad m => t m a -> m [a]
  toListT = foldMapT (: [])
