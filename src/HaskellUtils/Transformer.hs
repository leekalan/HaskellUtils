{-# LANGUAGE FunctionalDependencies, QuantifiedConstraints #-}
module HaskellUtils.Transformer where

class (forall m. Monad m => Monad (t m)) => MonadT t where
  lift :: Monad m => m a -> t m a

class (forall n. Monad n => Monad (t n)) => MonadE m t | m -> t where
  elev :: Monad n => m a -> t n a
