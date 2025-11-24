{-# LANGUAGE
  FunctionalDependencies, FlexibleInstances,
  InstanceSigs, TypeFamilies, RankNTypes
#-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-loopy-superclass-solve #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module HaskellUtils.Reader where

import HaskellUtils.Transformer
import Data.Kind

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
  type ElevMonad (Reader r) = ReaderT r

  elev :: Applicative n => Reader r a -> ReaderT r n a
  elev (Reader ra) = ReaderT $ \r -> pure $ ra r

instance IsElevMonad (ReaderT r) where
  type NonElevMonad (ReaderT r) = Reader r


class (Monad n, Monad m) => LiftReaderMonad n m where
  liftReader :: n a -> m a

-- This makes the boolean determined by n and m, creating injectivity
--
-- This is how the overlap error is avoided as this forces each instance of
-- LiftReaderMonad to be unique by providing an f to CreateMapRM
type family IsBaseCaseRM (n :: Type -> Type) (m :: Type -> Type) :: Bool where
  IsBaseCaseRM n n = 'True
  IsBaseCaseRM _ _ = 'False

-- This is a wrapper to store a function in respect to f
newtype MapRM (f :: Bool) n t (m :: Type -> Type) = MapRM (forall a. n a -> t m a)
runMapRM :: forall f n t m a. MapRM f n t m -> n a -> t m a
runMapRM (MapRM f) = f

class (Monad n, MonadT t, Monad m)
  => CreateMapRM f n t m where createMapRM :: MapRM f n t m

-- The base case
instance (Monad n, MonadT t, Monad m, n ~ m)
  => CreateMapRM 'True n t m where createMapRM = MapRM lift

-- The recursive case
instance (Monad n, MonadT t, Monad m, LiftReaderMonad n m)
  => CreateMapRM 'False n t m where createMapRM = MapRM $ lift . liftReader

-- CreateMapRM will always exist for some IsBaseCaseRM, this allows uniqueness
-- of implementation despite CreateMapRM not enforcing said uniqueness, this
-- basically uses the IsBaseCaseRM type family to determine which implementation
-- to use
instance CreateMapRM (IsBaseCaseRM n m) n t m => LiftReaderMonad n (t m) where
  liftReader :: n a -> t m a
  liftReader = runMapRM @(IsBaseCaseRM n m) createMapRM
