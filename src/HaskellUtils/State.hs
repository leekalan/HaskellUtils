{-# LANGUAGE
  TupleSections, FlexibleInstances, InstanceSigs,
  MultiParamTypeClasses, TypeFamilies, RankNTypes
#-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module HaskellUtils.State (
  StateMonad, sm_state, sm_put, sm_runState, sm_evalState, StateRet,
  state', get', gets', put', modify', runState', runStateF',
  evalState', evalStateF', evalValue', evalValueF',
  State, state, get, gets, put, runState, evalState,
  modify, runStateF, evalStateF, evalValue, evalValueF,
  StateT, stateT, getT, getsT, putT, runStateT, evalStateT,
  modifyT, runStateFT, evalStateFT, evalValueT, evalValueFT
) where

import Data.Bifunctor

import HaskellUtils.Transformer
import HaskellUtils.Environment
import HaskellUtils.Reader

class ReaderMonad s m => StateMonad s m where
  sm_state :: (s -> StateRet m (a, s)) -> m a
  sm_put :: s -> m ()

  sm_runState :: m a -> s -> StateRet m (a, s)
  sm_evalState :: m a -> s -> StateRet m s

type StateRet m a = ReaderRet m a

state' :: forall m s. StateMonad s m => forall a. (s -> StateRet m (a, s)) -> m a
state' = sm_state

get' :: StateMonad s m => m s
get' = ask'

gets' :: StateMonad s m => (s -> StateRet m a) -> m a
gets' = asks'

put' :: StateMonad s m => s -> m ()
put' = sm_put

runState' :: forall m s. StateMonad s m => forall a. m a -> s -> StateRet m (a, s)
runState' = sm_runState

evalState' :: forall m s. StateMonad s m => forall a. m a -> s -> StateRet m s
evalState' = sm_evalState

modify' :: forall m s. StateMonad s m => (s -> s) -> m ()
modify' f = get' >>= put' . f

runStateF' :: forall m s. StateMonad s m => forall a. s -> m a -> StateRet m (a, s)
runStateF' = flip runState'

evalStateF' ::forall m s.  StateMonad s m => forall a. s -> m a -> StateRet m s
evalStateF' = flip evalState'

evalValue' :: forall m s. StateMonad s m => forall a. m a -> s -> StateRet m a
evalValue' = runReader'

evalValueF' :: forall m s. StateMonad s m => forall a. s -> m a -> StateRet m a
evalValueF' = runReaderF'


newtype State s a = StateCons { _runState :: s -> (a, s) }

instance ReaderMonad s (State s) where
  rm_reader :: (s -> a) -> State s a
  rm_reader f = StateCons $ \s -> (, s) $ f s

  rm_ask :: State s s
  rm_ask = StateCons $ \s -> (s, s)

  rm_asks :: (s -> a) -> State s a
  rm_asks f = StateCons $ \s -> (f s, s)

  rm_runReader :: State s a -> s -> a
  rm_runReader s = fst . _runState s

instance StateMonad s (State s) where
  sm_state :: (s -> (a, s)) -> State s a
  sm_state = StateCons

  sm_put :: s -> State s ()
  sm_put s = StateCons $ const ((), s)

  sm_runState :: State s a -> s -> (a, s)
  sm_runState = _runState

  sm_evalState :: State s a -> s -> s
  sm_evalState sa = snd . _runState sa

state :: (s -> (a, s)) -> State s a
state = state'

get :: State s s
get = get'

gets :: (s -> a) -> State s a
gets = gets'

put :: s -> State s ()
put = put'

runState :: State s a -> s -> (a, s)
runState = runState'

evalState :: State s a -> s -> s
evalState = evalState'

evalValue :: State s a -> s -> a
evalValue = evalValue'

modify :: (s -> s) -> State s ()
modify = modify'

runStateF :: s -> State s a -> (a, s)
runStateF = runStateF'

evalStateF :: s -> State s a -> s
evalStateF = evalStateF'

evalValueF :: s -> State s a -> a
evalValueF = evalValueF'

instance MonadEnv s State where
  getEnv :: State s s
  getEnv = get'

  runWithEnv :: State s a -> s -> a
  runWithEnv = evalValue'

instance MonadEnvMut s State where
  setEnv :: s -> State s ()
  setEnv = put'

  runWithEnvMut :: State s a -> s -> (a, s)
  runWithEnvMut = runState'

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

instance Monad m => ReaderMonad s (StateT s m) where
  type ReaderRet (StateT s m) a = m a

  rm_reader :: (s -> m a) -> StateT s m a
  rm_reader f = StateTCons $ \s -> (, s) <$> f s

  rm_ask :: StateT s m s
  rm_ask = StateTCons $ \s -> return (s, s)

  rm_asks :: (s -> m a) -> StateT s m a
  rm_asks f = StateTCons $ \s -> (, s) <$> f s

  rm_runReader :: StateT s m a -> s -> m a
  rm_runReader sa s = fst <$> _runStateT sa s

instance Monad m => StateMonad s (StateT s m) where
  sm_state :: (s -> m (a, s)) -> StateT s m a
  sm_state = StateTCons

  sm_put :: s -> StateT s m ()
  sm_put s = StateTCons $ const $ return ((), s)

  sm_runState :: Monad m => StateT s m a -> s -> m (a, s)
  sm_runState = _runStateT

  sm_evalState :: Monad m => StateT s m a -> s -> m s
  sm_evalState sa s = snd <$> _runStateT sa s

stateT :: Monad m => (s -> m (a, s)) -> StateT s m a
stateT = state'

getT :: Monad m => StateT s m s
getT = get'

getsT :: Monad m => (s -> m a) -> StateT s m a
getsT = gets'

putT :: Monad m => s -> StateT s m ()
putT = put'

runStateT :: Monad m => StateT s m a -> s -> m (a, s)
runStateT = runState'

evalStateT :: Monad m => StateT s m a -> s -> m s
evalStateT = evalState'

modifyT :: Monad m => (s -> s) -> StateT s m ()
modifyT = modify'

runStateFT :: Monad m => s -> StateT s m a -> m (a, s)
runStateFT = runStateF'

evalStateFT :: Monad m => s -> StateT s m a -> m s
evalStateFT = evalStateF'

evalValueT :: Monad m => StateT s m a -> s -> m a
evalValueT = evalValue'

evalValueFT :: Monad m => s -> StateT s m a -> m a
evalValueFT = evalValueF'

instance MonadEnvT s StateT where
  getEnvT :: Monad m => StateT s m s
  getEnvT = get'

  runWithEnvT :: Monad m => StateT s m a -> s -> m a
  runWithEnvT = evalValue'

instance MonadEnvMutT s StateT where
  setEnvT :: Monad m => s -> StateT s m ()
  setEnvT = put'

  runWithEnvMutT :: Monad m => StateT s m a -> s -> m (a, s)
  runWithEnvMutT = runState'

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

instance MonadTMap (StateT s) where
  mapT :: (Monad m, Monad n) => (forall x. m x -> n x) -> StateT s m a -> StateT s n a
  mapT f (StateTCons ra) = StateTCons $ f . ra

instance MonadE (State s) where
  type ElevMonad (State s) = StateT s

  elev :: Applicative m => State s a -> StateT s m a
  elev (StateCons sa) = StateTCons $ \s -> pure $ sa s

instance IsElevMonad (StateT s) where
  type NonElevMonad (StateT s) = State s
