{-# LANGUAGE InstanceSigs, RankNTypes #-}
module HaskellUtils.PartialCont where

import HaskellUtils.Cont
import HaskellUtils.DelimCont
import HaskellUtils.Transformer

type ParBlock r = ParCont r r
type ParBlockT m r = ParContT r m r

type ParSeg r = ParCont r ()
type ParSegT m r = ParContT r m ()

type ParScope r = ParCont r
type ParScopeT m r = ParContT r m

type ParLoop r a = a -> ParCont r a
type ParLoopT m r a = a -> ParContT r m a


newtype ParCont r a = ParCont { _runParCont :: forall x. (a -> x) -> Cont x r }

parCont :: (forall x. (a -> x) -> Cont x r) -> ParCont r a
parCont = ParCont

runPar :: ParCont r a -> forall x. (a -> x) -> Cont x r
runPar = _runParCont

catchPar :: ParCont r r -> r
catchPar (ParCont ra) = catch $ ra id

catchParWith :: ParCont r a -> (a -> r) -> r
catchParWith (ParCont ra) f = catch $ ra f

throwPar :: r -> forall a. ParCont r a
throwPar r = parCont $ const $ return r

throwParEmpty :: r -> ParCont r ()
throwParEmpty = throwPar

runParM :: forall m r a. ParCont r a -> forall x. (a -> m x) -> ContT x m r
runParM ra f = asContT $ runPar ra f

runParMempty :: forall m r. (Applicative m, Monoid (m r))
  => forall a. ParCont r a -> m r
runParMempty r = catchT $ runParM r $ const mempty

instance Functor (ParCont r) where
  fmap :: (a -> b) -> ParCont r a -> ParCont r b
  fmap f (ParCont ra) = ParCont $ \k -> ra $ k . f

instance Applicative (ParCont r) where
  pure :: a -> ParCont r a
  pure a = ParCont $ throw . ($ a)

  (<*>) :: ParCont r (a -> b) -> ParCont r a -> ParCont r b
  (<*>) (ParCont rf) (ParCont ra) = ParCont $ \k -> cont $ \f ->
    runCont (rf $ \g -> runCont (ra $ k . g) f) f

instance Monad (ParCont r) where
  (>>=) :: ParCont r a -> (a -> ParCont r b) -> ParCont r b
  (>>=) (ParCont ra) far = ParCont $ \k -> cont $ \f ->
    runCont (ra $ \a -> runCont (runPar (far a) k) f) f

newtype ParContT r m a = ParContT { _runParContT :: forall x. (a -> m x) -> ContT x m r }

parContT :: (forall x. (a -> m x) -> ContT x m r) -> ParContT r m a
parContT = ParContT

runParT :: ParContT r m a -> forall x. (a -> m x) -> ContT x m r
runParT = _runParContT

catchParT :: Applicative m => ParContT r m r -> m r
catchParT (ParContT ra) = catchT $ ra pure

catchParWithT :: Applicative m => ParContT r m a -> (a -> m r) -> m r
catchParWithT (ParContT ra) f = catchT $ ra f

throwParT :: r -> forall a. ParContT r m a
throwParT r = parContT $ const $ return r

throwParEmptyT :: r -> ParContT r m ()
throwParEmptyT = throwParT

runParMT :: forall n m. (Monad m, MonadERun n)
  => forall r a. ParContT r m a -> forall b. (a -> n b) -> ContT b (ElevMonad n m) r
runParMT ra f = asContTNest $ runParT ra $ return . f

runParMemptyT :: forall n m r. (Monad m, MonadERun n, Monoid (n r))
  => forall a. ParContT r m a -> (ElevMonad n m) r
runParMemptyT r = catchT $ runParMT r $ const mempty

instance Functor (ParContT r m) where
  fmap :: (a -> b) -> ParContT r m a -> ParContT r m b
  fmap f (ParContT ra) = ParContT $ \k -> ra $ k . f

instance Applicative (ParContT r m) where
  pure :: a -> ParContT r m a
  pure a = ParContT $ \f -> throwM $ f a

  (<*>) :: ParContT r m (a -> b) -> ParContT r m a -> ParContT r m b
  (<*>) (ParContT rf) (ParContT ra) = ParContT $ \k -> contT $ \f ->
    runContT (rf $ \g -> runContT (ra $ k . g) f) f

instance Monad (ParContT r m) where
  (>>=) :: ParContT r m a -> (a -> ParContT r m b) -> ParContT r m b
  (>>=) (ParContT ra) far = ParContT $ \k -> contT $ \f ->
    runContT (ra $ \a -> runContT (runParT (far a) k) f) f



instance MonadT (ParContT r) where
  lift :: Monad m => m a -> ParContT r m a
  lift ma = ParContT $ \k -> throwM $ ma >>= k

asParContT :: Monad m => ParCont (m r) a -> ParContT r m a
asParContT (ParCont ra) = ParContT $ \k -> contT $ \f ->
  runCont (ra k) (>>= f)

parAsDelim :: ParCont r a -> DelimCont r a
parAsDelim (ParCont ra) = DelimCont $ \k -> cont $
  \f -> runCont (ra $ \a -> runCont (k a) f) f

parAsDelimT :: ParContT r m a -> DelimContT r m a
parAsDelimT (ParContT ra) = DelimContT $ \k -> contT $
  \f -> runContT (ra $ \a -> runContT (k a) f) f

delimAsPar :: DelimCont r a -> ParCont r a
delimAsPar (DelimCont ra) = ParCont $ \k -> cont $
  \f -> runCont (ra $ throw . k) f

delimAsParT :: DelimContT r m a -> ParContT r m a
delimAsParT (DelimContT ra) = ParContT $ \k -> contT $
  \f -> runContT (ra $ throwM . k) f
