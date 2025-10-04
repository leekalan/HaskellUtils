{-# LANGUAGE MultiParamTypeClasses #-}
module HaskellUtils.Environment where

import HaskellUtils.Transformer

class Monad (m r) => MonadEnv r m where
  getEnv :: m r r
  runWithEnv :: m r a -> r -> a

class MonadT (t r) => MonadEnvT r t where
  getEnvT :: Monad m => t r m r
  runWithEnvT :: Monad m => t r m a -> r -> m a

class  MonadEnv s m => MonadEnvMut s m where
  setEnv :: s -> m s ()
  runWithEnvMut :: m s a -> s -> (a, s)

class MonadEnvT s t => MonadEnvMutT s t where
  setEnvT :: Monad m => s -> t s m ()
  runWithEnvMutT :: Monad m => t s m a -> s -> m (a, s)

mapEnv :: (MonadEnv r m, MonadEnv k m) => (r -> k) -> m k a -> m r a
mapEnv f mka = fmap (runWithEnv mka . f) getEnv

mapEnvT :: Monad m => Monad (t r m) => (MonadEnvT r t, MonadEnvT k t) => (r -> k) -> t k m a -> t r m a
mapEnvT f tka = do
  r <- getEnvT
  lift $ runWithEnvT tka $ f r

mapEnvMut :: (MonadEnvMut r m, MonadEnvMut k m) => (r -> k) -> (k -> r -> r) -> m k a -> m r a
mapEnvMut f g mka = do
  r <- getEnv
  let (a, k) = runWithEnvMut mka $ f r
  setEnv $ g k r
  return a

mapEnvMutT :: Monad m => Monad (t r m) => (MonadEnvMutT r t, MonadEnvMutT k t) => (r -> k) -> (k -> r -> r) -> t k m a -> t r m a
mapEnvMutT f g tka = do
  r <- getEnvT
  (a, k) <- lift $ runWithEnvMutT tka $ f r
  setEnvT $ g k r
  return a
