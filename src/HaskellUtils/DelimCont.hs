{-# LANGUAGE
  InstanceSigs, FlexibleInstances,  FlexibleContexts,
  FunctionalDependencies, TypeFamilies, RankNTypes,
  ScopedTypeVariables, TypeApplications, TypeOperators,
  QuantifiedConstraints, AllowAmbiguousTypes
#-}
module HaskellUtils.DelimCont where

import HaskellUtils.Cont
import HaskellUtils.Transformer

type DelimBlock r = DelimCont r r
type DelimBlockT m r = ContT r m r

type DelimSeg r = DelimCont r ()
type DelimSegT m r = DelimContT r m ()

type DelimScope r = DelimCont r
type DelimScopeT m r = DelimContT r m

type DelimLoop r a = a -> DelimCont r a
type DelimLoopT m r a = a -> DelimContT r m a


newtype DelimCont r a = DelimCont { _runDelimCont :: forall x. (a -> Cont x r) -> Cont x r }

delimCont :: (forall x. (a -> Cont x r) -> Cont x r) -> DelimCont r a
delimCont = DelimCont

runDelim :: DelimCont r a -> forall x. (a -> Cont x r) -> Cont x r
runDelim = _runDelimCont

runDelimThrow :: DelimCont r a -> forall x. (a -> x) -> Cont x r
runDelimThrow ra f = runDelim ra $ throw . f

catchDelim :: DelimCont r r -> r
catchDelim (DelimCont ra) = catch $ ra return

catchDelimWith :: DelimCont r a -> (a -> r) -> r
catchDelimWith ra f = catchDelim $ f <$> ra

throwDelim :: r -> forall a. DelimCont r a
throwDelim r = delimCont $ const $ return r

throwDelimEmpty :: r -> DelimCont r ()
throwDelimEmpty = throwDelim

runDelimThrowM :: forall m. Monad m => forall r a. DelimCont r a -> forall x. (a -> m x) -> ContT x m r
runDelimThrowM ra f = asContT $ runDelimThrow ra f

runDelimThrowMempty :: forall m r. (Monad m, Monoid (m r))
  => forall a. DelimCont r a -> m r
runDelimThrowMempty r = catchT $ runDelimThrowM r $ const mempty

instance Functor (DelimCont r) where
  fmap :: (a -> b) -> DelimCont r a -> DelimCont r b
  fmap f (DelimCont ra) = DelimCont $ \k -> ra $ k . f

instance Applicative (DelimCont r) where
  pure :: a -> DelimCont r a
  pure a = DelimCont ($ a)

  (<*>) :: DelimCont r (a -> b) -> DelimCont r a -> DelimCont r b
  (<*>) (DelimCont rf) (DelimCont ra) = DelimCont $ \k ->
    rf $ \f -> ra $ k . f

instance Monad (DelimCont r) where
  (>>=) :: DelimCont r a -> (a -> DelimCont r b) -> DelimCont r b
  (>>=) (DelimCont ra) f = DelimCont $ \k ->
    ra $ \a -> runDelim (f a) k


newtype DelimContT r m a = DelimContT { _runDelimContT :: forall x. (a -> ContT x m r) -> ContT x m r }

delimContT :: (forall x. (a -> ContT x m r) -> ContT x m r) -> DelimContT r m a
delimContT = DelimContT

runDelimT :: DelimContT r m a -> forall x. (a -> ContT x m r) -> ContT x m r
runDelimT = _runDelimContT

runDelimThrowT :: Monad m => DelimContT r m a -> forall x. (a -> m x) -> ContT x m r
runDelimThrowT ra f = runDelimT ra $ throwM . f

catchDelimT :: Monad m => DelimContT r m r -> m r
catchDelimT (DelimContT ra) = catchT $ ra return

catchDelimWithT :: Monad m => DelimContT r m a -> (a -> m r) -> m r
catchDelimWithT ra f = catchDelimT $ ra >>= lift . f

throwDelimT :: Monad m => r -> forall a. DelimContT r m a
throwDelimT r = delimContT $ const $ return r

throwDelimEmptyT :: Monad m => r -> DelimContT r m ()
throwDelimEmptyT = throwDelimT

runDelimThrowMT :: forall n m. (Monad m, MonadERun n)
  => forall r a. DelimContT r m a -> forall b. (a -> n b) -> ContT b (ElevMonad n m) r
runDelimThrowMT ra f = asContTNest $ runDelimThrowT ra $ return . f

runDelimThrowMemptyT :: forall n m r. (Monad m, MonadERun n, Monoid (n r))
  => forall a. DelimContT r m a -> (ElevMonad n m) r
runDelimThrowMemptyT r = catchT $ runDelimThrowMT r $ const mempty

instance Functor (DelimContT r m) where
  fmap :: (a -> b) -> DelimContT r m a -> DelimContT r m b
  fmap f (DelimContT ra) = DelimContT $ \k -> ra $ k . f

instance Applicative (DelimContT r m) where
  pure :: a -> DelimContT r m a
  pure a = DelimContT ($ a)

  (<*>) :: DelimContT r m (a -> b) -> DelimContT r m a -> DelimContT r m b
  (<*>) (DelimContT rf) (DelimContT ra) =
    DelimContT $ \k ->
      rf $ \f ->
        ra $ k . f

instance Monad (DelimContT r m) where
  (>>=) :: DelimContT r m a -> (a -> DelimContT r m b) -> DelimContT r m b
  (>>=) (DelimContT ra) f =
    DelimContT $ \k ->
      ra $ \a ->
        runDelimT (f a) k


instance MonadT (DelimContT r) where
  lift :: Monad m => m a -> DelimContT r m a
  lift ma = DelimContT (lift ma >>=)

delimAsCont :: DelimCont r a -> forall x. ContT r (Cont x) a
delimAsCont (DelimCont ra) = contT ra

delimAsContT :: Monad m => DelimContT r m a -> forall x. ContT r (ContT x m) a
delimAsContT (DelimContT ra) = contT ra
