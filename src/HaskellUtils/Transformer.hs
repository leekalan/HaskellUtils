{-# LANGUAGE
  FunctionalDependencies, FlexibleContexts, TypeOperators,
  QuantifiedConstraints, TypeFamilies, RankNTypes
#-}
module HaskellUtils.Transformer where
  
import Data.Kind

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

class MonadT t => MonadTMap t where
  mapT :: (Monad m, Monad n) => (forall x. m x -> n x) -> t m a -> t n a

class (Monad m, MonadT (ElevMonad m), NonElevMonad (ElevMonad m) ~ m) => MonadE m where
  type ElevMonad m :: (Type -> Type) -> Type -> Type

  elev :: Monad n => m a -> ElevMonad m n a

  elevNest :: Monad n => n (m a) -> ElevMonad m n a
  elevNest mna = do
    na <- lift mna
    elev na

  elevFlip :: Monad n => m (n a) -> ElevMonad m n a
  elevFlip nma = do
    ma <- elev nma
    lift ma

class (MonadT t, MonadE (NonElevMonad t), ElevMonad (NonElevMonad t) ~ t) => IsElevMonad t where
  type NonElevMonad t :: (Type -> Type)

class MonadE m => MonadERun m where
  runElev :: ElevMonad m n a -> n (m a)

  runElevFlip :: MonadERun n => ElevMonad m n a -> m (n a)
  runElevFlip mna = runElev $ monadFlip mna

  monadNestFlip :: Monad n => m (n a) -> n (m a)
  monadNestFlip nma = runElev $ elevFlip nma

  monadFlip :: MonadE n => ElevMonad m n a -> ElevMonad n m a
  monadFlip mna = elevFlip $ runElev mna
