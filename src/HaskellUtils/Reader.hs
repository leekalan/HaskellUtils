{-# LANGUAGE
  FunctionalDependencies, FlexibleInstances,
  InstanceSigs, TypeFamilies, RankNTypes
#-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module HaskellUtils.Reader (
  ReaderMonad, rm_get, rm_runReader,
  get, gets, runReader, runReaderF, ReaderRet,
  Reader, reader, ReaderT, readerT
) where

import HaskellUtils.Transformer
import HaskellUtils.Environment

class Monad m => ReaderMonad r m | m -> r where
  type ReaderRet m a
  type ReaderRet m a = a

  rm_get :: m r
  rm_runReader :: m a -> r -> ReaderRet m a

get :: forall m r. ReaderMonad r m => m r
get = rm_get

runReader :: forall m r. ReaderMonad r m => forall a. m a -> r -> ReaderRet m a
runReader = rm_runReader

gets :: forall m r. ReaderMonad r m => forall a. (r -> a) -> m a
gets = (<$> get)

runReaderF :: forall m r. ReaderMonad r m => forall a. r -> m a -> ReaderRet m a
runReaderF = flip runReader


newtype Reader r a = ReaderCons { _runReader :: r -> a }

reader :: (r -> a) -> Reader r a
reader = ReaderCons

instance ReaderMonad r (Reader r) where
  rm_get :: Reader r r
  rm_get = ReaderCons id
  
  rm_runReader :: Reader r a -> r -> ReaderRet (Reader r) a
  rm_runReader = _runReader

instance MonadEnv r Reader where
  getEnv :: Reader r r
  getEnv = get

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


newtype ReaderT r m a = ReaderTCons { _runReaderT :: r -> m a }

readerT :: (r -> m a) -> ReaderT r m a
readerT = ReaderTCons

instance Monad m => ReaderMonad r (ReaderT r m) where
  type ReaderRet (ReaderT r m) a = m a

  rm_get :: Monad m => ReaderT r m r
  rm_get = ReaderTCons return

  rm_runReader :: ReaderT r m a -> r -> ReaderRet (ReaderT r m) a
  rm_runReader = _runReaderT


instance MonadEnvT r ReaderT where
  getEnvT :: Monad m => ReaderT r m r
  getEnvT = get

  runWithEnvT :: Monad m => ReaderT r m a -> r -> m a
  runWithEnvT = runReader

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
    runReader (f a) r


instance MonadT (ReaderT r) where
  lift :: Monad m => m a -> ReaderT r m a
  lift m = ReaderTCons $ const m

instance MonadE (Reader r) (ReaderT r) where
  elev :: Applicative n => Reader r a -> ReaderT r n a
  elev (ReaderCons ra) = ReaderTCons $ \r -> pure $ ra r
