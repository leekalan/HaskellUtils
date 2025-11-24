{-# LANGUAGE
  FunctionalDependencies, RankNTypes,
  FlexibleInstances, TypeFamilies,
  TupleSections, InstanceSigs
#-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module HaskellUtils.State where

import Data.Bifunctor

import HaskellUtils.Transformer
import HaskellUtils.Reader

class ReaderMonad s m => StateMonad s m | m -> s where
  state' :: (s -> StateRet m (a, s)) -> m a
  put :: s -> m ()

  runState' :: m a -> s -> StateRet m (a, s)
  execState' :: m a -> s -> StateRet m s

  get :: m s
  get = ask

  gets :: MonadRet ar (m a) => (s -> ar) -> m a
  gets = asks

  modify :: (s -> s) -> m ()
  modify f = get >>= put . f

  evalState' :: m a -> s -> StateRet m a
  evalState' = runReader'

  runStateF' :: s -> m a -> StateRet m (a, s)
  runStateF' = flip runState'

  execStateF' :: s -> m a -> StateRet m s
  execStateF' = flip execState'

  evalStateF' :: s -> m a -> StateRet m a
  evalStateF' = flip evalState'

type StateRet m a = ReaderRet m a


newtype State s a = State { runState :: s -> (a, s) }

state :: (s -> (a, s)) -> State s a
state = State

execState :: State s a -> s -> s
execState sa = snd . runState sa

evalState :: State s a -> s -> a
evalState sa = fst . runState sa

runStateF :: s -> State s a -> (a, s)
runStateF = flip runState

execStateF :: s -> State s a -> s
execStateF = flip execState

evalStateF :: s -> State s a -> a
evalStateF = flip evalState

instance ReaderMonad s (State s) where
  type ReaderRet (State s) a = a

  reader' :: (s -> a) -> State s a
  reader' f = State $ \s -> (f s, s)

  ask :: State s s
  ask = State $ \s -> (s, s)

  runReader' :: State s a -> s -> a
  runReader' = evalState

instance StateMonad s (State s) where
  state' :: (s -> (a, s)) -> State s a
  state' = State

  put :: s -> State s ()
  put s = State $ const ((), s)

  runState' :: State s a -> s -> (a, s)
  runState' = runState

  execState' :: State s a -> s -> s
  execState' = execState

instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  fmap f (State sa) = State $ \s ->
    let (a, s') = sa s
    in  (f a, s')

instance Applicative (State s) where
  pure :: a -> State s a
  pure a = State (a,)

  (<*>) :: State s (a -> b) -> State s a -> State s b
  (<*>) (State sf) (State sa) = State $ \s ->
    let (f, s') = sf s
    in  first f $ sa s'

instance Monad (State s) where
  (>>=) :: State s a -> (a -> State s b) -> State s b
  (>>=) (State sa) f = State $ \s ->
    let (a, s') = sa s
        (State sb) = f a
    in  sb s'


newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

stateT :: (s -> m (a, s)) -> StateT s m a
stateT = StateT

execStateT :: Functor m => StateT s m a -> s -> m s
execStateT sa = fmap snd . runStateT sa

evalStateT :: Functor m => StateT s m a -> s -> m a
evalStateT sa = fmap fst . runStateT sa

runStateTF :: s -> StateT s m a -> m (a, s)
runStateTF = flip runStateT

execStateTF :: Functor m => s -> StateT s m a -> m s
execStateTF = flip execStateT

evalStateTF :: Functor m => s -> StateT s m a -> m a
evalStateTF = flip evalStateT

instance Monad m => ReaderMonad s (StateT s m) where
  type ReaderRet (StateT s m) a = m a

  reader' :: (s -> m a) -> StateT s m a
  reader' f = StateT $ \s -> fmap (, s) (f s)

  ask :: StateT s m s
  ask = StateT $ \s -> return (s, s)

  runReader' :: StateT s m a -> s -> m a
  runReader' = evalStateT

instance Functor m => Functor (StateT s m) where
  fmap :: (a -> b) -> StateT s m a -> StateT s m b
  fmap f (StateT sa) = StateT $ \s ->
    let m = sa s
    in  fmap (first f) m

instance Monad m => Applicative (StateT s m) where
  pure :: a -> StateT s m a
  pure a = StateT $ \s -> pure (a, s)

  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  (<*>) (StateT sf) (StateT sa) = StateT $ \s -> do
    (f, s') <- sf s
    first f <$> sa s'

instance Monad m => Monad (StateT s m) where
  (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  (>>=) (StateT sa) f = StateT $ \s -> do
    (a, s') <- sa s
    runStateT (f a) s'


instance MonadT (StateT s) where
  lift :: Monad m => m a -> StateT s m a
  lift m = StateT $ \s -> do
    a <- m
    return (a, s)

instance MonadTMap (StateT s) where
  mapT :: (Monad m, Monad n) => (forall x. m x -> n x) -> StateT s m a -> StateT s n a
  mapT f (StateT ra) = StateT $ f . ra

instance MonadE (State s) where
  type ElevMonad (State s) = StateT s

  elev :: Applicative m => State s a -> StateT s m a
  elev (State sa) = StateT $ \s -> pure $ sa s

instance IsElevMonad (StateT s) where
  type NonElevMonad (StateT s) = State s
