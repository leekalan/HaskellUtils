{-# LANGUAGE
  TypeFamilies, TypeOperators, FlexibleInstances, UndecidableInstances,
  QuantifiedConstraints, FlexibleContexts, MultiParamTypeClasses, RankNTypes,
  TypeApplications, ScopedTypeVariables, InstanceSigs, AllowAmbiguousTypes
#-}
{-# OPTIONS_GHC -Wno-loopy-superclass-solve #-}
{-# LANGUAGE ConstraintKinds #-}
module HaskellUtils.Transformer where

import Data.Kind
import Data.Functor.Identity

nop :: Monad m => m ()
nop = return ()


newtype IdentityT m a = IdentityT { runIdentityT :: m a }
identityT :: m a -> IdentityT m a
identityT = IdentityT

instance Monad m => Functor (IdentityT m) where
  fmap f (IdentityT m) = IdentityT (fmap f m)

instance Monad m => Applicative (IdentityT m) where
  pure = IdentityT . pure
  (<*>) (IdentityT mf) (IdentityT ma) = IdentityT (mf <*> ma)

instance Monad m => Monad (IdentityT m) where
  return = pure
  (>>=) (IdentityT ma) f = IdentityT (ma >>= runIdentityT . f)

instance MonadT IdentityT where
  lift :: Monad m => m a -> IdentityT m a
  lift = IdentityT

instance MonadTMap IdentityT where
  mapT :: (Monad m, Monad n) => (forall x. m x -> n x) -> IdentityT m a -> IdentityT n a
  mapT f (IdentityT ma) = IdentityT $ f ma

instance MonadE Identity where
  type EMonad Identity = IdentityT

  elev :: Monad n => Identity a -> EMonad Identity n a
  elev a = identityT $ return $ runIdentity a

instance UnMonadE IdentityT where
  type UnEMonad IdentityT = Identity

instance MonadERun Identity where
  runElev :: Monad n => IdentityT n a -> n (Identity a)
  runElev (IdentityT ma) = fmap Identity ma


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

type LiftMonadSelf (m :: Type -> Type) = LiftMonad m m

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

-- Identity lift
instance Monad n => LiftMonad n n where
  liftR :: Monad n => n a -> n a
  liftR = id

-- Index the implementation with the case
instance (MonadT t, LiftMonad' (LiftCase n m) n m) => LiftMonad n (t m) where
  liftR :: n a -> t m a
  liftR = lift . liftR' @(LiftCase n m)


class (Monad m, Monad (LiftTrans t m))
  => LiftMonadT m (t :: (Type -> Type) -> Type -> Type) where
  type LiftTrans t m :: Type -> Type

  liftT :: m a -> LiftTrans t m a

type IdentityLift = IdentityT

newtype ComposeT
  (u :: (Type -> Type) -> Type -> Type)
  (v :: (Type -> Type) -> Type -> Type)
  (m :: Type -> Type) a
  = ComposeT (u (v m) a)
type ComposeT3 u v w = ComposeT u (ComposeT v w)
type ComposeT4 u v w x = ComposeT u (ComposeT3 v w x)
type ComposeT5 u v w x y = ComposeT u (ComposeT4 v w x y)

type family LiftCaseT (t :: (Type -> Type) -> Type -> Type) where
  LiftCaseT IdentityLift = IdentityCase
  LiftCaseT (ComposeT u v) = Recurse
  LiftCaseT _ = Base

data IdentityCase

class LiftMonadT' c m (t :: (Type -> Type) -> Type -> Type) where
  type LiftTrans' c t m :: Type -> Type
  liftT' :: m a -> LiftTrans' c t m a

instance Monad m => LiftMonadT' IdentityCase m IdentityLift where
  type LiftTrans' IdentityCase IdentityLift m = m

  liftT' :: m a -> m a
  liftT' = id

instance (Monad m, MonadT t) => LiftMonadT' Base m t where
  type LiftTrans' Base t m = t m

  liftT' :: m a -> t m a
  liftT' = lift

instance (
  LiftMonadT m v,
  LiftMonadT (LiftTrans v m) u
  )
  => LiftMonadT' Recurse m (ComposeT u v) where
  type LiftTrans' Recurse (ComposeT u v) m = LiftTrans2 u v m

  liftT' :: m a -> LiftTrans2 u v m a
  liftT' = liftT @(LiftTrans v m) @u . liftT @m @v

instance (Monad m, Monad (LiftTrans' (LiftCaseT t) t m), LiftMonadT' (LiftCaseT t) m t) => LiftMonadT m t where
  type LiftTrans t m = LiftTrans' (LiftCaseT t) t m

  liftT :: m a -> LiftTrans t m a
  liftT = liftT' @(LiftCaseT t) @m @t

type LiftTrans2 u v m = LiftTrans u (LiftTrans v m)
type LiftTrans3 u v w m = LiftTrans u (LiftTrans2 v w m)
type LiftTrans4 u v w x m = LiftTrans u (LiftTrans3 v w x m)
type LiftTrans5 u v w x y m = LiftTrans u (LiftTrans4 v w x y m)

type LiftComposed u v m = (
  LiftMonadT m v,
  LiftMonadT m (ComposeT u v),
  LiftMonadT (LiftTrans v m) u,
  LiftTrans (ComposeT u v) m ~ LiftTrans2 u v m
  ) :: Constraint
type LiftComposed3 u v w m = (
  LiftComposed v w m,
  LiftComposed (ComposeT u v) w m,
  LiftComposed u (ComposeT v w) m,
  LiftComposed u v (LiftTrans w m),
  LiftTrans (ComposeT3 u v w) m ~ LiftTrans3 u v w m
  ) :: Constraint
-- type LiftComposed4 u v w x m = (
--   LiftComposed3 u v w (LiftTrans x m),
--   LiftComposed3 (ComposeT u v) w x m,
--   ComposeT4 u v w x m ~ LiftTrans4 u v w x m) :: Constraint
-- type LiftComposed5 u v w x y m = (
--   LiftComposed4 u v w x (LiftTrans y m),
--   LiftComposed4 (ComposeT u v) w x y m,
--   ComposeT5 u v w x y m ~ LiftTrans5 u v w x y m) :: Constraint

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
  runElev :: Monad n => EMonad m n a -> n (m a)

  runElevFlip :: MonadERun n => EMonad m n a -> m (n a)
  runElevFlip mna = runElev $ monadFlip mna

  monadNestFlip :: Monad n => m (n a) -> n (m a)
  monadNestFlip nma = runElev $ elevFlip nma

  monadFlip :: MonadE n => EMonad m n a -> EMonad n m a
  monadFlip mna = elevFlip $ runElev mna
