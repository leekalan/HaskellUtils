{-# LANGUAGE
  UndecidableInstances, InstanceSigs, RankNTypes,
  MultiParamTypeClasses, TypeFamilies
#-}
{-# OPTIONS_GHC -Wno-orphans #-}
module HaskellUtils.Either (
  EitherT(EitherT), runEitherT, leftT, rightT, eitherT, liftLeft,
  mapLeft, mapRight, mapLeftT, mapRightT,
  (~>>=)
) where

import Data.Bifunctor (first, second)

import HaskellUtils.Transformer

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance (Eq (m (Either e a))) => Eq (EitherT e m a) where
  (==) :: EitherT e m a -> EitherT e m a -> Bool
  (==) (EitherT ma) (EitherT mb) = ma == mb

instance (Ord (m (Either e a))) => Ord (EitherT e m a) where
  compare :: EitherT e m a -> EitherT e m a -> Ordering
  compare (EitherT ma) (EitherT mb) = compare ma mb

instance Functor m => Functor (EitherT e m) where
  fmap :: (a -> b) -> EitherT e m a -> EitherT e m b
  fmap f (EitherT ma) = EitherT $ fmap (fmap f) ma

mapLeft :: (e -> b) -> Either e a -> Either b a
mapLeft = first

mapRight :: (a -> b) -> Either e a -> Either e b
mapRight = second

mapLeftT :: Functor m => (e -> b) -> EitherT e m a -> EitherT b m a
mapLeftT f (EitherT ma) = EitherT $ fmap (mapLeft f) ma

mapRightT :: Functor m => (a -> b) -> EitherT e m a -> EitherT e m b
mapRightT f (EitherT ma) = EitherT $ fmap (mapRight f) ma

instance Monad m => Applicative (EitherT e m) where
  pure :: a -> EitherT e m a
  pure a = EitherT $ pure $ Right a

  (<*>) :: Monad m => EitherT e m (a -> b) -> EitherT e m a -> EitherT e m b
  (<*>) (EitherT rf) (EitherT ra) = EitherT $ do
    f <- rf
    case f of
      Left e -> return $ Left e
      Right f' -> fmap (fmap f') ra

instance Monad m => Monad (EitherT e m) where
  (>>=) :: EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
  (>>=) (EitherT ra) f = EitherT $ do
    a <- ra
    case a of
      Left e -> return $ Left e
      Right a' -> runEitherT $ f a'

(~>>=) :: Monad m => EitherT e m a -> (e -> EitherT b m a) -> EitherT b m a
(~>>=) (EitherT ra) f = EitherT $ do
  a <- ra
  case a of
    Left e -> runEitherT $ f e
    Right a' -> return $ Right a'

leftT :: Applicative m => e -> EitherT e m a
leftT e = EitherT $ pure $ Left e

rightT :: Applicative m => a -> EitherT e m a
rightT a = EitherT $ pure $ Right a

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT onLeft onRight m = do
  e <- runEitherT m
  case e of
    Left a -> onLeft a
    Right b -> onRight b

instance MonadT (EitherT e) where
  lift :: Monad m => m a -> EitherT e m a
  lift m = EitherT $ Right <$> m
  
instance MonadTMap (EitherT e) where
  mapT :: (Monad m, Monad n) => (forall x. m x -> n x) -> EitherT e m a -> EitherT e n a
  mapT f (EitherT ma) = EitherT $ f ma

liftLeft :: Monad m => m e -> EitherT e m a
liftLeft e = EitherT $ Left <$> e

instance MonadE (Either e) where
  type ElevMonad (Either e) = EitherT e

  elev :: Monad n => Either e a -> EitherT e n a
  elev a = EitherT $ pure a
