{-# LANGUAGE InstanceSigs, RankNTypes #-}
module HaskellUtils.PartialCont where

import HaskellUtils.Cont
import HaskellUtils.DelimCont
import HaskellUtils.Transformer
import Control.Applicative

type ParBlock r = ParCont r r
type ParBlockT m r = ParContT r m r

type ParSeg = ParCont ()
type ParSegT m r = ParContT r m ()


newtype ParCont s a = ParCont { _runParCont :: forall x. (s -> x) -> Cont x a }

parCont :: (forall x. (s -> x) -> Cont x a) -> ParCont s a
parCont = ParCont

runPar :: ParCont s a -> forall x. (s -> x) -> Cont x a
runPar = _runParCont

runPar' :: (s -> x) -> forall a. ParCont s a -> Cont x a
runPar' = flip _runParCont

runParM :: ParCont s a -> forall m x. (s -> m x) -> ContT x m a
runParM ra f = asContT $ runPar ra f

runParEmpty :: ParCont s a -> forall m. Alternative m => m a
runParEmpty ra = catchT $ runParM ra $ const empty

instance Functor (ParCont s) where
  fmap :: (a -> b) -> ParCont s a -> ParCont s b
  fmap f (ParCont sa) = ParCont $ fmap f . sa

instance Applicative (ParCont s) where
  pure :: a -> ParCont s a
  pure a = ParCont $ const $ return a

  (<*>) :: ParCont s (a -> b) -> ParCont s a -> ParCont s b
  (<*>) (ParCont sf) (ParCont sa) = ParCont $ \k -> sf k <*> sa k

instance Monad (ParCont s) where
  (>>=) :: ParCont s a -> (a -> ParCont s b) -> ParCont s b
  (>>=) (ParCont sa) fsb = ParCont $ \k -> sa k >>= runPar' k . fsb


newtype ParContT s m a = ParContT { _runParContT :: forall x. (s -> m x) -> ContT x m a }

parContT :: (forall x. (s -> m x) -> ContT x m a) -> ParContT s m a
parContT = ParContT

runParT :: ParContT s m a -> forall x. (s -> m x) -> ContT x m a
runParT = _runParContT

runParT' :: (s -> m x) -> ParContT s m a -> ContT x m a
runParT' = flip _runParContT

runParMT :: Monad m => ParContT s m a -> forall n. MonadERun n
  => forall x. (s -> (EMonad n m) x) -> ContT x (EMonad n m) a
runParMT ra f = asContTNest $ runParT ra $ runElev . f

runParEmptyT :: Monad m => ParContT s m a
  -> forall n. (MonadERun n, Alternative n) => (EMonad n m) a
runParEmptyT ra = catchT $ runParMT ra $ const $ elev empty

instance Functor (ParContT s m) where
  fmap :: (a -> b) -> ParContT s m a -> ParContT s m b
  fmap f (ParContT sa) = ParContT $ fmap f . sa

instance Applicative (ParContT s m) where
  pure :: a -> ParContT s m a
  pure a = ParContT $ const $ return a

  (<*>) :: ParContT s m (a -> b) -> ParContT s m a -> ParContT s m b
  (<*>) (ParContT sf) (ParContT sa) = ParContT $ \k -> sf k <*> sa k

instance Monad (ParContT s m) where
  (>>=) :: ParContT s m a -> (a -> ParContT s m b) -> ParContT s m b
  (>>=) (ParContT sa) fsb = ParContT $ \k -> sa k >>= runParT' k . fsb


instance MonadT (ParContT s) where
  lift :: Monad m => m a -> ParContT s m a
  lift ma = ParContT $ const $ lift ma

asParContT :: Monad m => ParCont (m s) a -> ParContT s m a
asParContT (ParCont ra) = ParContT $ \k -> asContT $ ra (>>= k)

asParContTAlt :: Monad m => ParCont s (m a) -> ParContT s m a
asParContTAlt (ParCont ra) = ParContT $ \k -> asContT (ra k) >>= lift

parAsDelim :: ParCont a r -> DelimCont r a
parAsDelim (ParCont ra) = DelimCont $ \k -> cont $
  \f -> runCont (ra $ \a -> runCont (k a) f) f

parAsDelimT :: ParContT a m r -> DelimContT r m a
parAsDelimT (ParContT ra) = DelimContT $ \k -> contT $
  \f -> runContT (ra $ \a -> runContT (k a) f) f

delimAsPar :: DelimCont a s -> ParCont s a
delimAsPar ra = ParCont $ runDelimThrow ra

delimAsParT :: DelimContT a m s -> ParContT s m a
delimAsParT ra = ParContT $ runDelimThrowT ra
