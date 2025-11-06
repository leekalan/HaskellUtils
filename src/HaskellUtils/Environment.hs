{-# LANGUAGE FunctionalDependencies #-}
module HaskellUtils.Environment where

import HaskellUtils.Transformer

class Monad (m r) => MonadEnv r m where
  getEnv :: m r r
  runWithEnv :: m r a -> r -> a

class MonadT (t r) => MonadEnvT r t where
  getEnvT :: Monad m => t r m r
  runWithEnvT :: Monad m => t r m a -> r -> m a

class MonadEnv s m => MonadEnvMut s m where
  setEnv :: s -> m s ()
  runWithEnvMut :: m s a -> s -> (a, s)

class MonadEnvT s t => MonadEnvMutT s t where
  setEnvT :: Monad m => s -> t s m ()
  runWithEnvMutT :: Monad m => t s m a -> s -> m (a, s)

modifyEnv :: MonadEnvMut r m => (r -> r) -> m r ()
modifyEnv f = getEnv >>= setEnv . f

modifyEnvT :: (Monad m, MonadEnvMutT r t) => (r -> r) -> t r m ()
modifyEnvT f = getEnvT >>= setEnvT . f

mapEnv :: (MonadEnv r m, MonadEnv k m)
  => (r -> k) -> m k a -> m r a
mapEnv f mka = fmap (runWithEnv mka . f) getEnv

mapEnvT :: Monad m => Monad (t r m) => (MonadEnvT r t, MonadEnvT k t)
  => (r -> k) -> t k m a -> t r m a
mapEnvT f tka = do
  r <- getEnvT
  lift $ runWithEnvT tka $ f r

mapEnvMut :: (MonadEnvMut r m, MonadEnvMut k m)
  => (r -> k) -> (k -> r -> r) -> m k a -> m r a
mapEnvMut f g mka = do
  r <- getEnv
  let (a, k) = runWithEnvMut mka $ f r
  setEnv $ g k r
  return a

mapEnvMutT :: Monad m => Monad (t r m) => (MonadEnvMutT r t, MonadEnvMutT k t)
  => (r -> k) -> (k -> r -> r) -> t k m a -> t r m a
mapEnvMutT f g tka = do
  r <- getEnvT
  (a, k) <- lift $ runWithEnvMutT tka $ f r
  setEnvT $ g k r
  return a

class Field r a | r -> a where
  getField :: r -> a
  setField :: a -> r -> r

  modifyField :: (a -> a) -> r -> r
  modifyField f r = setField (f $ getField r) r

fieldEnv :: Field r k => (MonadEnv r m, MonadEnv k m)
  => m k a -> m r a
fieldEnv = mapEnv getField

fieldEnvT :: Field r k => Monad m => Monad (t r m) => (MonadEnvT r t, MonadEnvT k t)
  => t k m a -> t r m a
fieldEnvT = mapEnvT getField

fieldEnvMut :: Field r k => (MonadEnvMut r m, MonadEnvMut k m)
  => m k a -> m r a
fieldEnvMut = mapEnvMut getField setField

fieldEnvMutT :: Field r k => Monad m => Monad (t r m) => (MonadEnvMutT r t, MonadEnvMutT k t)
  => t k m a -> t r m a
fieldEnvMutT = mapEnvMutT getField setField
