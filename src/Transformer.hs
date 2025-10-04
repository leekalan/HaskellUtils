{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuantifiedConstraints #-}
module Transformer where

class MonadT t where
  lift :: Monad m => m a -> t m a

class MonadE m t | m -> t where
  elev :: Monad n => m a -> t n a
