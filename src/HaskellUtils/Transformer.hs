{-# LANGUAGE
  TypeFamilies, TypeOperators, FlexibleInstances, UndecidableInstances,
  QuantifiedConstraints, FlexibleContexts, MultiParamTypeClasses, RankNTypes,
  TypeApplications, ScopedTypeVariables, InstanceSigs, AllowAmbiguousTypes
#-}
{-# OPTIONS_GHC -Wno-loopy-superclass-solve #-}
module HaskellUtils.Transformer where
  
import Data.Kind

nop :: Monad m => m ()
nop = return ()


class MonadRet a m where
  liftRet :: a -> m

instance Monad m => MonadRet a (m a) where
  liftRet :: a -> m a
  liftRet = return

instance (Monad m, MonadT t) => MonadRet (m a) (t m a) where
  liftRet :: m a -> t m a
  liftRet = lift

instance (Monad m, MonadT t, MonadRet ar (m a)) => MonadRet ar (t m a) where
  liftRet :: ar -> t m a
  liftRet = lift . liftRet


class (forall m. Monad m => Monad (t m)) => MonadT t where
  lift :: Monad m => m a -> t m a

lift2 :: (Monad m, MonadT t0, MonadT t1) => m a -> t0 (t1 m) a
lift2 = lift . lift

lift3 :: (Monad m, MonadT t0, MonadT t1, MonadT t2) => m a -> t0 (t1 (t2 m)) a
lift3 = lift . lift2

lift4 :: (Monad m, MonadT t0, MonadT t1, MonadT t2, MonadT t3) => m a -> t0 (t1 (t2 (t3 m))) a
lift4 = lift . lift3

lift5 :: (Monad m, MonadT t0, MonadT t1, MonadT t2, MonadT t3, MonadT t4) => m a -> t0 (t1 (t2 (t3 (t4 m)))) a
lift5 = lift . lift4

lift6 :: (Monad m, MonadT t0, MonadT t1, MonadT t2, MonadT t3, MonadT t4, MonadT t5) => m a -> t0 (t1 (t2 (t3 (t4 (t5 m))))) a
lift6 = lift . lift5

lift7 :: (Monad m, MonadT t0, MonadT t1, MonadT t2, MonadT t3, MonadT t4, MonadT t5, MonadT t6) => m a -> t0 (t1 (t2 (t3 (t4 (t5 (t6 m)))))) a
lift7 = lift . lift6


class (Monad n, Monad m) => LiftMonad n m where
  liftR :: n a -> m a

type family LiftCase (n :: Type -> Type) (m :: Type -> Type) where
  LiftCase n n = Base
  LiftCase _ _ = Recurse

data Base
data Recurse

class (Monad n, Monad m) => LiftMonad' c n m where
  liftR' :: n a -> m a

-- Lift when in base case
instance (Monad m)
  => LiftMonad' Base m m where
  liftR' :: m a -> m a
  liftR' = id

-- Recurse when in recurse case
instance (Monad n, Monad m, LiftMonad n m)
  => LiftMonad' Recurse n m where
  liftR' :: n a -> m a
  liftR' = liftR

-- Base case
instance Monad n => LiftMonad n n where
  liftR :: Monad n => n a -> n a
  liftR = id

-- Index the implementation with the case
instance (MonadT t, LiftMonad' (LiftCase n m) n m) => LiftMonad n (t m) where
  liftR :: n a -> t m a
  liftR = lift . liftR' @(LiftCase n m)


class MonadT t => MonadTMap t where
  mapT :: (Monad m, Monad n) => (forall x. m x -> n x) -> t m a -> t n a

class (Monad m, MonadT (EMonad m), UnEMonad (EMonad m) ~ m) => MonadE m where
  type EMonad m :: (Type -> Type) -> Type -> Type

  elev :: Monad n => m a -> EMonad m n a

  elevNest :: Monad n => n (m a) -> EMonad m n a
  elevNest mna = do
    na <- lift mna
    elev na

  elevFlip :: Monad n => m (n a) -> EMonad m n a
  elevFlip nma = do
    ma <- elev nma
    lift ma

class (MonadT t, MonadE (UnEMonad t), EMonad (UnEMonad t) ~ t) => UnMonadE t where
  type UnEMonad t :: Type -> Type

class MonadE m => MonadERun m where
  runElev :: EMonad m n a -> n (m a)

  runElevFlip :: MonadERun n => EMonad m n a -> m (n a)
  runElevFlip mna = runElev $ monadFlip mna

  monadNestFlip :: Monad n => m (n a) -> n (m a)
  monadNestFlip nma = runElev $ elevFlip nma

  monadFlip :: MonadE n => EMonad m n a -> EMonad n m a
  monadFlip mna = elevFlip $ runElev mna
