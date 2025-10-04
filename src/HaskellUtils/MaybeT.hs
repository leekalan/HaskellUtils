{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module HaskellUtils.MaybeT (MaybeT, runMaybeT, justT, nothingT, maybeT) where

import HaskellUtils.Transformer

newtype MaybeT m a = MaybeTCons { runMaybeT :: m (Maybe a) }

instance Functor m => Functor (MaybeT m) where
  fmap :: (a -> b) -> MaybeT m a -> MaybeT m b
  fmap f (MaybeTCons ma) = MaybeTCons $ fmap (fmap f) ma

instance Monad m => Applicative (MaybeT m) where
  pure :: a -> MaybeT m a
  pure a = MaybeTCons $ pure $ Just a

  (<*>) :: MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b
  (<*>) (MaybeTCons mf) (MaybeTCons ma) = MaybeTCons $ do
    f <- mf
    case f of
      Nothing -> return Nothing
      Just f' -> fmap (fmap f') ma

instance Monad m => Monad (MaybeT m) where
  (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  (>>=) (MaybeTCons ma) f = MaybeTCons $ do
    a <- ma
    case a of
      Nothing -> return Nothing
      Just a' -> runMaybeT $ f a'

justT :: Applicative m => a -> MaybeT m a
justT a = MaybeTCons $ pure $ Just a

nothingT :: Applicative m => MaybeT m a
nothingT = MaybeTCons $ pure Nothing

maybeT :: Monad m => m b -> (a -> m b) -> MaybeT m a -> m b
maybeT onNothing onJust (MaybeTCons ma) = ma >>= maybe onNothing onJust

instance MonadT MaybeT where
  lift :: Monad m => m a -> MaybeT m a
  lift m = MaybeTCons $ fmap Just m

instance MonadE Maybe MaybeT where
  elev :: Monad n => Maybe a -> MaybeT n a
  elev ma = MaybeTCons $ pure ma
