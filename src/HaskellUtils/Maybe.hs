{-# LANGUAGE
  MultiParamTypeClasses, InstanceSigs,
  UndecidableInstances, TypeFamilies, RankNTypes
#-}
{-# OPTIONS_GHC -Wno-orphans #-}
module HaskellUtils.Maybe (
  MaybeT(MaybeT), runMaybeT, justT, nothingT, maybeT,
  liftNothing, onNothingT, (!>>=)
) where

import GHC.Base (Alternative(..))

import HaskellUtils.Transformer

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance (Eq (m (Maybe a))) => Eq (MaybeT m a) where
  (==) :: Eq (m (Maybe a)) => MaybeT m a -> MaybeT m a -> Bool
  (==) (MaybeT ma) (MaybeT mb) = ma == mb

instance (Ord (m (Maybe a))) => Ord (MaybeT m a) where
  compare :: Ord (m (Maybe a)) => MaybeT m a -> MaybeT m a -> Ordering
  compare (MaybeT ma) (MaybeT mb) = compare ma mb

instance Functor m => Functor (MaybeT m) where
  fmap :: (a -> b) -> MaybeT m a -> MaybeT m b
  fmap f (MaybeT ma) = MaybeT $ fmap (fmap f) ma

instance Monad m => Applicative (MaybeT m) where
  pure :: a -> MaybeT m a
  pure a = MaybeT $ pure $ Just a

  (<*>) :: MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b
  (<*>) (MaybeT mf) (MaybeT ma) = MaybeT $ do
    f <- mf
    case f of
      Nothing -> return Nothing
      Just f' -> fmap (fmap f') ma

instance Monad m => Monad (MaybeT m) where
  (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  (>>=) (MaybeT ma) f = MaybeT $ do
    a <- ma
    case a of
      Nothing -> return Nothing
      Just a' -> runMaybeT $ f a'

instance Monad m => Alternative (MaybeT m) where
  empty :: MaybeT m a
  empty = nothingT

  (<|>) :: MaybeT m a -> MaybeT m a -> MaybeT m a
  (<|>) (MaybeT ma) (MaybeT mb) = MaybeT $ do
    r <- ma
    case r of
      Nothing -> mb
      Just _ -> return r

instance (Monad m, Semigroup a) => Semigroup (MaybeT m a) where
  (<>) :: MaybeT m a -> MaybeT m a -> MaybeT m a
  (<>) (MaybeT ma) (MaybeT mb) = MaybeT $ do
    a <- ma
    b <- mb
    return $ a <> b

instance (Monad m, Semigroup a) => Monoid (MaybeT m a) where
  mempty :: MaybeT m a
  mempty = nothingT

justT :: Applicative m => a -> MaybeT m a
justT a = MaybeT $ pure $ Just a

nothingT :: Applicative m => MaybeT m a
nothingT = MaybeT $ pure Nothing

maybeT :: Monad m => m b -> (a -> m b) -> MaybeT m a -> m b
maybeT onNothing onJust (MaybeT ma) = ma >>= maybe onNothing onJust

instance MonadT MaybeT where
  lift :: Monad m => m a -> MaybeT m a
  lift m = MaybeT $ fmap Just m

instance MonadTMap MaybeT where
  mapT :: (Monad m, Monad n) => (forall x. m x -> n x) -> MaybeT m a -> MaybeT n a
  mapT f (MaybeT ma) = MaybeT $ f ma

liftNothing :: Monad m => m () -> MaybeT m a
liftNothing m = MaybeT $ do
  m
  return Nothing

instance MonadE Maybe where
  type ElevMonad Maybe = MaybeT

  elev :: Monad n => Maybe a -> MaybeT n a
  elev ma = MaybeT $ pure ma

instance IsElevMonad MaybeT where
  type NonElevMonad MaybeT = Maybe

instance MonadERun Maybe where
  runElev :: MaybeT n a -> n (Maybe a)
  runElev (MaybeT ma) = ma

onNothingT :: Monad m => m () -> MaybeT m a -> MaybeT m a
onNothingT m (MaybeT ma) = MaybeT $ do
  r <- ma
  case r of
    Nothing -> do
      m
      return Nothing
    Just _ -> return r

(!>>=) :: Monad m => MaybeT m a -> m () -> MaybeT m a
(!>>=) = flip onNothingT
