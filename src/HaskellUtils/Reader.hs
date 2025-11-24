{-# LANGUAGE
  FunctionalDependencies, FlexibleInstances,
  InstanceSigs, TypeFamilies, RankNTypes
#-}
module HaskellUtils.Reader where

import HaskellUtils.Transformer

class Monad m => ReaderMonad r m | m -> r where
  type ReaderRet m a

  reader' :: (r -> ReaderRet m a) -> m a
  ask :: m r
  runReader' :: m a -> r -> ReaderRet m a

  asks :: MonadRet ar (m a) => (r -> ar) -> m a
  asks f = ask >>= liftRet . f

  runReaderF' :: r -> m a -> ReaderRet m a
  runReaderF' = flip runReader'


newtype Reader r a = Reader { runReader :: r -> a }

reader :: (r -> a) -> Reader r a
reader = Reader

runReaderF :: r -> Reader r a -> a
runReaderF = flip runReader

instance ReaderMonad r (Reader r) where
  type ReaderRet (Reader r) a = a

  reader' :: (r -> a) -> Reader r a
  reader' = Reader

  ask :: Reader r r
  ask = Reader id

  runReader' :: Reader r a -> r -> a
  runReader' = runReader

instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader ra) = Reader $ f . ra

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ const a

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (<*>) (Reader rf) (Reader ra) = Reader $ \r -> rf r $ ra r

instance Monad (Reader r) where
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (>>=) (Reader ra) f = Reader $ \r -> runReader (f $ ra r) r


newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

readerT :: (r -> m a) -> ReaderT r m a
readerT = ReaderT

runReaderFT :: r -> ReaderT r m a -> m a
runReaderFT = flip runReaderT

instance Monad m => ReaderMonad r (ReaderT r m) where
  type ReaderRet (ReaderT r m) a = m a

  reader' :: (r -> m a) -> ReaderT r m a
  reader' = ReaderT

  ask :: Monad m => ReaderT r m r
  ask = ReaderT return

  runReader' :: ReaderT r m a -> r -> m a
  runReader' = runReaderT

instance Functor m => Functor (ReaderT r m) where
  fmap :: (a -> b) -> ReaderT r m a -> ReaderT r m b
  fmap f (ReaderT ra) = ReaderT $ fmap f . ra

instance Monad m => Applicative (ReaderT r m) where
  pure :: a -> ReaderT r m a
  pure a = ReaderT $ const $ pure a

  (<*>) :: ReaderT r m (a -> b) -> ReaderT r m a -> ReaderT r m b
  (<*>) (ReaderT rf) (ReaderT ra) = ReaderT $ \r -> do
    f <- rf r
    a <- ra r
    return $ f a

instance Monad m => Monad (ReaderT r m) where
  (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
  (>>=) (ReaderT ra) f = ReaderT $ \r -> do
    a <- ra r
    runReaderT (f a) r


instance MonadT (ReaderT r) where
  lift :: Monad m => m a -> ReaderT r m a
  lift m = ReaderT $ const m

instance MonadTMap (ReaderT r) where
  mapT :: (Monad m, Monad n) => (forall x. m x -> n x) -> ReaderT r m a -> ReaderT r n a
  mapT f (ReaderT ra) = ReaderT $ f . ra

instance MonadE (Reader r) where
  type EMonad (Reader r) = ReaderT r

  elev :: Applicative n => Reader r a -> ReaderT r n a
  elev (Reader ra) = ReaderT $ \r -> pure $ ra r

instance UnMonadE (ReaderT r) where
  type UnEMonad (ReaderT r) = Reader r
