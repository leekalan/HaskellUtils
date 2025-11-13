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

runDelimWith :: DelimCont r a -> forall x. (a -> Cont x r) -> Cont x r
runDelimWith = _runDelimCont

runDelim :: DelimCont r r -> forall x. Cont x r
runDelim (DelimCont ra) = ra return

throwDelim :: r -> forall a. DelimCont r a
throwDelim r = delimCont $ const $ return r

throwDelimEmpty :: r -> DelimCont r ()
throwDelimEmpty = throwDelim

runDelimM :: forall m. Monad m => forall r a. DelimCont r a -> forall b. (a -> m b) -> ContT b m r
runDelimM ra f = catchT $ delimAsContM ra >>= lift . throwM . f

runDelimMempty :: forall m r. (Monad m, Monoid (m r))
  => DelimCont r () -> m r
runDelimMempty r = catchT $ runDelimM r $ const mempty

instance Functor (DelimCont r) where
  fmap :: (a -> b) -> DelimCont r a -> DelimCont r b
  fmap f (DelimCont ra) = DelimCont $ \k -> ra $ k . f

instance Applicative (DelimCont r) where
  pure :: a -> DelimCont r a
  pure a = DelimCont $ \k -> k a

  (<*>) :: DelimCont r (a -> b) -> DelimCont r a -> DelimCont r b
  (<*>) (DelimCont rf) (DelimCont ra) = DelimCont $ \k ->
    rf $ \f -> ra $ k . f

instance Monad (DelimCont r) where
  (>>=) :: DelimCont r a -> (a -> DelimCont r b) -> DelimCont r b
  (>>=) (DelimCont ra) f = DelimCont $ \k ->
    ra $ \a -> runDelimWith (f a) k


newtype DelimContT r m a = DelimContT { _runDelimContT :: forall x. (a -> ContT x m r) -> ContT x m r }

delimContT :: (forall x. (a -> ContT x m r) -> ContT x m r) -> DelimContT r m a
delimContT = DelimContT

runDelimWithT :: DelimContT r m a -> forall x. (a -> ContT x m r) -> ContT x m r
runDelimWithT = _runDelimContT

runDelimT :: Monad m => DelimContT r m r -> forall x. ContT x m r
runDelimT (DelimContT ra) = ra return

throwDelimT :: Monad m => r -> forall a. DelimContT r m a
throwDelimT r = DelimContT $ const $ return r

throwDelimEmptyT :: Monad m => r -> DelimContT r m ()
throwDelimEmptyT = throwDelimT

runDelimMT :: forall n m. (Monad m, MonadERun n)
  => forall r a. DelimContT r m a -> forall b. (a -> n b) -> ContT b (ElevMonad n m) r
runDelimMT ra f = catchT $ delimAsContTM ra
  >>= lift . throwM . elev . f

runDelimMemptyT :: forall n m r. (Monad m, MonadERun n, Monoid (n r))
  => DelimContT r m () -> (ElevMonad n m) r
runDelimMemptyT r = catchT $ runDelimMT r $ const mempty

instance Functor (DelimContT r m) where
  fmap :: (a -> b) -> DelimContT r m a -> DelimContT r m b
  fmap f (DelimContT ra) = DelimContT $ \k -> ra $ k . f

instance Applicative (DelimContT r m) where
  pure :: a -> DelimContT r m a
  pure a = DelimContT $ \k -> k a

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
        runDelimWithT (f a) k


instance MonadT (DelimContT r) where
  lift :: Monad m => m a -> DelimContT r m a
  lift ma = DelimContT (lift ma >>=)

delimAsCont :: DelimCont r a -> forall x. ContT r (Cont x) a
delimAsCont (DelimCont ra) = contT ra

delimAsContM :: DelimCont r a -> forall m x. Monad m => ContT r (ContT x m) a
delimAsContM (DelimCont ra) = contT $ \k -> asContT $ ra $ asCont . k

delimAsContT :: Monad m => DelimContT r m a -> forall x. ContT r (ContT x m) a
delimAsContT (DelimContT ra) = contT ra

delimAsContTM :: Monad m => DelimContT r m a -> forall n x. MonadERun n
  => ContT r (ContT x (ElevMonad n m)) a
delimAsContTM (DelimContT ra) = contT $ \k ->
  -- This is converting the 'k' into what is expected by 'ra'
  let ta = ra $ \a -> contT $ \h -> runElev $ runContT (k a) $ elevNest . h
  -- This is converting the result of 'ra' into the expected result of 'k'
  in  contT $ \c -> elevNest $ runContT ta $ runElev . c
