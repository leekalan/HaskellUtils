{-# LANGUAGE
  TupleSections, FlexibleInstances, InstanceSigs,
  MultiParamTypeClasses, TypeFamilies, RankNTypes
#-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module HaskellUtils.State (
  StateMonad, sm_put, sm_runState, sm_evalState,
  put, modify, runState, runStateF,
  evalState, evalStateF, evalValue, evalValueF, StateRet,
  State, state, StateT, stateT,
) where

import Data.Bifunctor

import HaskellUtils.Transformer
import HaskellUtils.Environment
import HaskellUtils.Reader

class ReaderMonad s m => StateMonad s m where
  sm_put :: s -> m ()

  sm_runState :: m a -> s -> StateRet m (a, s)
  sm_evalState :: m a -> s -> StateRet m s

put :: StateMonad s m => s -> m ()
put = sm_put

runState :: forall m s. StateMonad s m => forall a. m a -> s -> StateRet m (a, s)
runState = sm_runState

evalState :: forall m s. StateMonad s m => forall a. m a -> s -> StateRet m s
evalState = sm_evalState

modify :: forall m s. StateMonad s m => (s -> s) -> m ()
modify f = get >>= put . f

runStateF :: forall m s. StateMonad s m => forall a. s -> m a -> StateRet m (a, s)
runStateF = flip runState

evalStateF ::forall m s.  StateMonad s m => forall a. s -> m a -> StateRet m s
evalStateF = flip evalState

evalValue :: forall m s. StateMonad s m => forall a. m a -> s -> StateRet m a
evalValue = runReader

evalValueF :: forall m s. StateMonad s m => forall a. s -> m a -> StateRet m a
evalValueF = runReaderF

type StateRet m a = ReaderRet m a


newtype State s a = StateCons { _runState :: s -> (a, s) }

state :: (s -> (a, s)) -> State s a
state = StateCons

instance ReaderMonad s (State s) where
  rm_get :: State s s
  rm_get = StateCons $ \s -> (s, s)

  rm_runReader :: State s a -> s -> a
  rm_runReader s = fst . _runState s

instance StateMonad s (State s) where
  sm_put :: s -> State s ()
  sm_put s = StateCons $ const ((), s)
  
  sm_runState :: State s a -> s -> (a, s)
  sm_runState = _runState

  sm_evalState :: State s a -> s -> s
  sm_evalState sa = snd . _runState sa

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


newtype StateT s m a = StateTCons { _runStateT :: s -> m (a, s) }

stateT :: (s -> m (a, s)) -> StateT s m a
stateT = StateTCons

instance Monad m => ReaderMonad s (StateT s m) where
  type ReaderRet (StateT s m) a = m a

  rm_get :: StateT s m s
  rm_get = StateTCons $ \s -> return (s, s)

  rm_runReader :: StateT s m a -> s -> m a
  rm_runReader sa s = fst <$> _runStateT sa s

instance Monad m => StateMonad s (StateT s m) where
  sm_put :: s -> StateT s m ()
  sm_put s = StateTCons $ const $ return ((), s)

  sm_runState :: Monad m => StateT s m a -> s -> m (a, s)
  sm_runState = _runStateT
  
  sm_evalState :: Monad m => StateT s m a -> s -> m s
  sm_evalState sa s = snd <$> _runStateT sa s

instance MonadEnvT s StateT where
  getEnvT :: Monad m => StateT s m s
  getEnvT = get

  runWithEnvT :: Monad m => StateT s m a -> s -> m a
  runWithEnvT = evalValue

instance MonadEnvMutT s StateT where
  setEnvT :: Monad m => s -> StateT s m ()
  setEnvT = put

  runWithEnvMutT :: Monad m => StateT s m a -> s -> m (a, s)
  runWithEnvMutT = runState

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
    runState (f a) s'


instance MonadT (StateT s) where
  lift :: Monad m => m a -> StateT s m a
  lift m = StateTCons $ \s -> do
    a <- m
    return (a, s)

instance MonadE (State s) (StateT s) where
  elev :: Applicative m => State s a -> StateT s m a
  elev (StateCons sa) = StateTCons $ \s -> pure $ sa s

