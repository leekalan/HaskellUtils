{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module HaskellUtils.Reader (Reader, runReader, ReaderT, runReaderT) where

import HaskellUtils.Transformer
import HaskellUtils.Environment

class Monad m => MonadReader r m | m -> r where
  ask :: m r

  asks :: (r -> a) -> m a
  asks = (<$> ask)

newtype Reader r a = ReaderCons { runReader :: r -> a }

instance MonadReader r (Reader r) where
  ask :: Reader r r
  ask = ReaderCons id

instance MonadEnv r Reader where
  getEnv :: Reader r r
  getEnv = ask

  runWithEnv :: Reader r a -> r -> a
  runWithEnv = runReader

instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (ReaderCons ra) = ReaderCons $ f . ra

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = ReaderCons $ const a

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (<*>) (ReaderCons rf) (ReaderCons ra) = ReaderCons $ \r -> rf r $ ra r

instance Monad (Reader r) where
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (>>=) (ReaderCons ra) f = ReaderCons $ \r -> runReader (f $ ra r) r


newtype ReaderT r m a = ReaderTCons { runReaderT :: r -> m a }

instance Monad m => MonadReader r (ReaderT r m) where
  ask :: Monad m => ReaderT r m r
  ask = ReaderTCons return

instance MonadEnvT r ReaderT where
  getEnvT :: Monad m => ReaderT r m r
  getEnvT = ask

  runWithEnvT :: Monad m => ReaderT r m a -> r -> m a
  runWithEnvT = runReaderT

instance Functor m => Functor (ReaderT r m) where
  fmap :: (a -> b) -> ReaderT r m a -> ReaderT r m b
  fmap f (ReaderTCons ra) = ReaderTCons $ fmap f . ra

instance Monad m => Applicative (ReaderT r m) where
  pure :: a -> ReaderT r m a
  pure a = ReaderTCons $ const $ pure a

  (<*>) :: ReaderT r m (a -> b) -> ReaderT r m a -> ReaderT r m b
  (<*>) (ReaderTCons rf) (ReaderTCons ra) = ReaderTCons $ \r -> do
    f <- rf r
    a <- ra r
    return $ f a

instance Monad m => Monad (ReaderT r m) where
  (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
  (>>=) (ReaderTCons ra) f = ReaderTCons $ \r -> do
    a <- ra r
    runReaderT (f a) r


instance MonadT (ReaderT r) where
  lift :: Monad m => m a -> ReaderT r m a
  lift m = ReaderTCons $ const m

instance MonadE (Reader r) (ReaderT r) where
  elev :: Applicative n => Reader r a -> ReaderT r n a
  elev (ReaderCons ra) = ReaderTCons $ \r -> pure $ ra r
