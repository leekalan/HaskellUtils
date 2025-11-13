{-# LANGUAGE
  FunctionalDependencies, FlexibleInstances,
  InstanceSigs, TypeFamilies, RankNTypes
#-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module HaskellUtils.Reader (
  ReaderMonad, rm_reader, rm_ask, rm_asks, rm_runReader, ReaderRet,
  reader', ask', asks', runReader', runReaderF',
  Reader, reader, ask, asks, runReader, runReaderF,
  ReaderT, readerT, askT, asksT, runReaderT, runReaderFT,
) where

import HaskellUtils.Transformer
import HaskellUtils.Environment

class Monad m => ReaderMonad r m | m -> r where
  type ReaderRet m a
  type ReaderRet m a = a

  rm_reader :: (r -> ReaderRet m a) -> m a
  rm_ask :: m r
  rm_asks :: (r -> ReaderRet m a) -> m a
  rm_runReader :: m a -> r -> ReaderRet m a

reader' :: forall m r. ReaderMonad r m => forall a. (r -> ReaderRet m a) -> m a
reader' = rm_reader

ask' :: forall m r. ReaderMonad r m => m r
ask' = rm_ask

runReader' :: forall m r. ReaderMonad r m => forall a. m a -> r -> ReaderRet m a
runReader' = rm_runReader

asks' :: forall m r. ReaderMonad r m => forall a. (r -> ReaderRet m a) -> m a
asks' = rm_asks

runReaderF' :: forall m r. ReaderMonad r m => forall a. r -> m a -> ReaderRet m a
runReaderF' = flip runReader'


newtype Reader r a = ReaderCons { _runReader :: r -> a }

instance ReaderMonad r (Reader r) where
  rm_reader :: (r -> a) -> Reader r a
  rm_reader = ReaderCons

  rm_ask :: Reader r r
  rm_ask = ReaderCons id

  rm_asks :: (r -> a) -> Reader r a
  rm_asks = ReaderCons
  
  rm_runReader :: Reader r a -> r -> ReaderRet (Reader r) a
  rm_runReader = _runReader

reader :: (r -> a) -> Reader r a
reader = reader'

ask :: Reader r r
ask = ask'

runReader :: Reader r a -> r -> a
runReader = runReader'

asks :: (r -> a) -> Reader r a
asks = asks'

runReaderF :: r -> Reader r a -> a
runReaderF = runReaderF'

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


newtype ReaderT r m a = ReaderTCons { _runReaderT :: r -> m a }

instance Monad m => ReaderMonad r (ReaderT r m) where
  type ReaderRet (ReaderT r m) a = m a

  rm_reader :: (r -> m a) -> ReaderT r m a
  rm_reader = ReaderTCons

  rm_ask :: Monad m => ReaderT r m r
  rm_ask = ReaderTCons return

  rm_asks :: (r -> m a) -> ReaderT r m a
  rm_asks = ReaderTCons

  rm_runReader :: ReaderT r m a -> r -> ReaderRet (ReaderT r m) a
  rm_runReader = _runReaderT

readerT :: Monad m => (r -> m a) -> ReaderT r m a
readerT = reader'

askT :: Monad m => ReaderT r m r
askT = ask'

runReaderT :: Monad m => ReaderT r m a -> r -> ReaderRet (ReaderT r m) a
runReaderT = runReader'

asksT :: Monad m => (r -> m a) -> ReaderT r m a
asksT = asks'

runReaderFT :: Monad m => r -> ReaderT r m a -> ReaderRet (ReaderT r m) a
runReaderFT = runReaderF'

instance MonadEnvT r ReaderT where
  getEnvT :: Monad m => ReaderT r m r
  getEnvT = askT

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

instance MonadTMap (ReaderT r) where
  mapT :: (Monad m, Monad n) => (forall x. m x -> n x) -> ReaderT r m a -> ReaderT r n a
  mapT f (ReaderTCons ra) = ReaderTCons $ f . ra

instance MonadE (Reader r) where
  type ElevMonad (Reader r) = ReaderT r

  elev :: Applicative n => Reader r a -> ReaderT r n a
  elev (ReaderCons ra) = ReaderTCons $ \r -> pure $ ra r

instance IsElevMonad (ReaderT r) where
  type NonElevMonad (ReaderT r) = Reader r
