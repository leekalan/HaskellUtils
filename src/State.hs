{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module State (MonadState, get, put, gets, modify, evalState, evalValue, State, runState, StateT, runStateT) where

import Data.Bifunctor

import Transformer
import Environment

class Monad m => MonadState s m | m -> s where
  get :: m s
  put :: s -> m ()

  gets :: (s -> a) -> m a
  gets f = fmap f get

  modify :: (s -> s) -> m ()
  modify f = get >>= put . f

newtype State s a = StateCons { runState :: s -> (a, s) }

instance MonadState s (State s) where
  get :: State s s
  get = StateCons $ \s -> (s, s)

  put :: s -> State s ()
  put s = StateCons $ const ((), s)

evalState :: State s a -> s -> s
evalState (StateCons sa) = snd . sa

evalValue :: State s a -> s -> a
evalValue (StateCons sa) = fst . sa

instance MonadEnv s State where
  getEnv :: State s s
  getEnv = get

  runWithEnv :: State s a -> s -> a
  runWithEnv = evalValue

instance MonadEnvMut s State where
  setEnv :: s -> State s ()
  setEnv = put

  runWithEnvMut :: State s a -> s -> (a, s)
  runWithEnvMut = runState

instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  fmap f (StateCons sa) = StateCons $ \s ->
    let (a, s') = sa s
    in  (f a, s')

instance Applicative (State s) where
  pure :: a -> State s a
  pure a = StateCons (a,)

  (<*>) :: State s (a -> b) -> State s a -> State s b
  (<*>) (StateCons sf) (StateCons sa) = StateCons $ \s ->
    let (f, s') = sf s
    in  first f $ sa s'

instance Monad (State s) where
  (>>=) :: State s a -> (a -> State s b) -> State s b
  (>>=) (StateCons sa) f = StateCons $ \s ->
    let (a, s') = sa s
        (StateCons sb) = f a
    in  sb s'


newtype StateT s m a = StateTCons { runStateT :: s -> m (a, s) }

instance Monad m => MonadState s (StateT s m) where
  get :: StateT s m s
  get = StateTCons $ \s -> pure (s, s)

  put :: s -> StateT s m ()
  put s = StateTCons $ const $ pure ((), s)

evalStateT :: Functor m => StateT s m a -> s -> m a
evalStateT (StateTCons sa) = fmap fst . sa

evalValueT :: Functor m => StateT s m a -> s -> m a
evalValueT (StateTCons sa) = fmap fst . sa

instance MonadEnvT s StateT where
  getEnvT :: Monad m => StateT s m s
  getEnvT = get

  runWithEnvT :: Monad m => StateT s m a -> s -> m a
  runWithEnvT = evalStateT

instance MonadEnvMutT s StateT where
  setEnvT :: Monad m => s -> StateT s m ()
  setEnvT = put

  runWithEnvMutT :: Monad m => StateT s m a -> s -> m (a, s)
  runWithEnvMutT = runStateT

instance Functor m => Functor (StateT s m) where
  fmap :: (a -> b) -> StateT s m a -> StateT s m b
  fmap f (StateTCons sa) = StateTCons $ \s ->
    let m = sa s
    in  fmap (first f) m

instance Monad m => Applicative (StateT s m) where
  pure :: a -> StateT s m a
  pure a = StateTCons $ \s -> pure (a, s)

  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  (<*>) (StateTCons sf) (StateTCons sa) = StateTCons $ \s -> do
    (f, s') <- sf s
    first f <$> sa s'

instance Monad m => Monad (StateT s m) where
  (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  (>>=) (StateTCons sa) f = StateTCons $ \s -> do
    (a, s') <- sa s
    runStateT (f a) s'


instance MonadT (StateT s) where
  lift :: Monad m => m a -> StateT s m a
  lift m = StateTCons $ \s -> do
    a <- m
    return (a, s)

instance MonadE (State s) (StateT s) where
  elev :: Applicative m => State s a -> StateT s m a
  elev (StateCons sa) = StateTCons $ \s -> pure $ sa s

