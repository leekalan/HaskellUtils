{-# LANGUAGE
  FunctionalDependencies, FlexibleContexts,
  QuantifiedConstraints, TypeFamilies, RankNTypes
#-}
{-# OPTIONS_GHC -Wno-star-is-type #-}
module HaskellUtils.Transformer where

class (forall m. Monad m => Monad (t m)) => MonadT t where
  lift :: Monad m => m a -> t m a

lift2 :: (Monad m, MonadT t0, MonadT t1) => m a -> t0 (t1 m) a
lift2 = lift . lift

lift3 :: (Monad m, MonadT t0, MonadT t1, MonadT t2) => m a -> t0 (t1 (t2 m)) a
lift3 = lift . lift2

lift4 :: (Monad m, MonadT t0, MonadT t1, MonadT t2, MonadT t3) => m a -> t0 (t1 (t2 (t3 m))) a
lift4 = lift . lift3

lift5 :: (Monad m, MonadT t0, MonadT t1, MonadT t2, MonadT t3, MonadT t4) => m a -> t0 (t1 (t2 (t3 (t4 m)))) a
lift5 = lift . lift4

lift6 :: (Monad m, MonadT t0, MonadT t1, MonadT t2, MonadT t3, MonadT t4, MonadT t5) => m a -> t0 (t1 (t2 (t3 (t4 (t5 m))))) a
lift6 = lift . lift5

lift7 :: (Monad m, MonadT t0, MonadT t1, MonadT t2, MonadT t3, MonadT t4, MonadT t5, MonadT t6) => m a -> t0 (t1 (t2 (t3 (t4 (t5 (t6 m)))))) a
lift7 = lift . lift6

class MonadT t => MonadTMap t where
  mapT :: (Monad m, Monad n) => (forall x. m x -> n x) -> t m a -> t n a

class (Monad m, MonadT (ElevMonad m)) => MonadE m where
  type ElevMonad m :: (* -> *) -> * -> *

  elev :: Monad n => m a -> ElevMonad m n a
