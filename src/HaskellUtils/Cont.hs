{-# LANGUAGE
  InstanceSigs, FlexibleInstances, TypeOperators,
  FunctionalDependencies, TypeFamilies, RankNTypes,
  ScopedTypeVariables, TypeApplications
#-}
module HaskellUtils.Cont (
  Block, BlockT, Seg, SegT, Scope, ScopeT, Loop, LoopT, asCont, asContT, asContTNest, unContTNest, asContTFlip, unContTFlip,
  ContMonad, ContRet, cm_cont, cm_runCont, cm_runContId,
  cont', runCont', runContId', throw', throwEmpty', throwEmptyM', mapResult', catch', catchM', catchL', recurse', recurseF', loop', loopF',
  Cont, cont, runCont, runContId, throw, throwEmpty, mapResult, catch, catchM, recurse, recurseF, loop, loopF,
  ContT, contT, runContT, runContIdT, throwT, throwM, throwEmptyT, mapResultT, throwEmptyM, catchT, catchL, catchMonoid, recurseT, recurseFT, loopT, loopFT, loopState,
) where

import HaskellUtils.Transformer
import HaskellUtils.State

type Block r = Cont r r
type BlockT m r = ContT r m r

type Seg r = Cont r ()
type SegT m r = ContT r m ()

type Scope r = Cont r
type ScopeT m r = ContT r m

type Loop r a = a -> Cont r a
type LoopT m r a = a -> ContT r m a

{-|
  The class describes a monad where we can pass in a continuation
  from a -> r to extract an r. The monad operates on the value of
  a.
-}
class Monad m => ContMonad r m | m -> r where
  type ContRet m r
  type ContRet m r = r

  cm_cont :: ((a -> ContRet m r) -> ContRet m r) -> m a
  cm_runCont :: m a -> (a -> ContRet m r) -> ContRet m r
  cm_runContId :: m r -> ContRet m r

cont' :: forall m r. ContMonad r m => forall a. ((a -> ContRet m r) -> ContRet m r) -> m a
cont' = cm_cont

runCont' :: forall m r. ContMonad r m => forall a. m a -> (a -> ContRet m r) -> ContRet m r
runCont' = cm_runCont

runContId' :: forall m r. ContMonad r m => m r -> ContRet m r
runContId' = cm_runContId

throw' :: forall m r. ContMonad r m => forall a. r -> m a
throw' r = cont' $ const $ runContId' @m $ return r

throwM' :: forall m r. ContMonad r m => forall a. ContRet m r -> m a
throwM' r = cont' $ const r

throwEmpty' :: forall m r. ContMonad r m => r -> m ()
throwEmpty' = throw'

throwEmptyM' :: forall m r. ContMonad r m => ContRet m r -> m ()
throwEmptyM' = throwM'

mapResult' :: forall m m' r r'. (ContMonad r m, ContMonad r' m')
  => (ContRet m r -> ContRet m' r') -> (ContRet m' r' -> ContRet m r)
  -> forall a. m a -> m' a
mapResult' f g ra = cont' $ \k -> f $ runCont' ra $ g . k

catch' :: forall m r. ContMonad r m => m r -> ContRet m r
catch' = runContId'

catchM' :: forall m r n. (ContMonad r m, Monad n)
  => forall a. a ~ ContRet m r => m r -> n a
catchM' ra = return $ catch' ra

catchL' :: forall m r t n. (ContMonad r m, Monad n, MonadT t)
  => forall a. n a ~ ContRet m r => m r -> t n a
catchL' ra = lift $ catch' ra

recurse' :: forall m r a. ContMonad r m => (a -> m a) -> a -> m r
recurse' f a = f a >>= recurse' f

recurseF' :: forall m r a. ContMonad r m => a -> (a -> m a) -> m r
recurseF' = flip recurse'

loop' :: forall m r a. ContMonad r m => (a -> m a) -> a -> ContRet m r
loop' f = catch' . recurse' f

loopF' :: forall m r a. ContMonad r m => a -> (a -> m a) -> ContRet m r
loopF' = flip loop'

-- The function from (a -> r) -> r contains a state
-- which either applys the function a -> r to an a or
-- returns the already calculated r
newtype Cont r a = Cont { _runCont :: (a -> r) -> r }

instance ContMonad r (Cont r) where
  type ContRet (Cont r) r = r

  cm_cont :: ((a -> r) -> r) -> Cont r a
  cm_cont = Cont

  cm_runCont :: Cont r a -> (a -> r) -> r
  cm_runCont = _runCont

  cm_runContId :: Cont r r -> r
  cm_runContId m = runCont' m id

cont :: ((a -> r) -> r) -> Cont r a
cont = cont'

runCont :: Cont r a -> (a -> r) -> r
runCont = runCont'

runContId :: Cont r r -> r
runContId = runContId'

throw :: r -> forall a. Cont r a
throw = throw'

throwEmpty :: r -> Cont r ()
throwEmpty = throwEmpty'

mapResult :: (r -> r') -> (r' -> r) -> Cont r a -> Cont r' a
mapResult = mapResult'

catch :: Cont r r -> r
catch = catch'

catchM :: Cont r r -> Cont r' r
catchM = catchM'

recurse :: (a -> Cont r a) -> a -> Cont r r
recurse = recurse'

recurseF :: a -> (a -> Cont r a) -> Cont r r
recurseF = recurseF'

loop :: (a -> Cont r a) -> a -> r
loop = loop'

loopF :: a -> (a -> Cont r a) -> r
loopF = loopF'

instance Functor (Cont r) where
  -- We have a function from a -> b and a function from
  -- (a -> r) -> r. This means that by chaining
  -- a -> b into the continuation b -> r we get an a -> r
  -- which can be passed to the function from (a -> r) -> r
  fmap :: (a -> b) -> Cont r a -> Cont r b
  fmap f (Cont ra) = Cont $ \k -> ra (k . f)

instance Applicative (Cont r) where
  -- Simply stores an a to apply the function a -> r to
  pure :: a -> Cont r a
  pure a = Cont ($ a)

  -- To extract r from 'Cont r a' we need a -> r. By extracting a -> b
  -- we can chain it into b -> r to get a -> r. This can then be passed
  -- to 'Cont r a', giving us an r.
  (<*>) :: Cont r (a -> b) -> Cont r a -> Cont r b
  (<*>) (Cont rf) (Cont ra) =
    Cont $ \k ->    -- the continuation where k :: b -> r
      rf $ \f ->    -- the continuation where f :: a -> b
        ra $ k . f  -- chaining a -> b into b -> r is passed into ra
                    -- this finally returns an r

instance Monad (Cont r) where
  -- To extract r from b -> r we can pass b -> r into a 'Cont r b'.
  -- To get 'Cont r b' we need a to pass into f.
  (>>=) :: Cont r a -> (a -> Cont r b) -> Cont r b
  (>>=) (Cont ra) f =
    Cont $ \k ->        -- the continuation where k :: b -> r
      ra $ \a ->        -- the continuation where a :: a
        runCont' (f a) k -- 'f a' gives us 'Cont r b' for which k is passed
                        -- this gives us an r


newtype ContT r m a = ContT { _runContT :: (a -> m r) -> m r }

contT :: Monad m => ((a -> m r) -> m r) -> ContT r m a
contT = cont'

runContT :: Monad m => ContT r m a -> (a -> m r) -> m r
runContT = runCont'

runContIdT :: Monad m => ContT r m r -> m r
runContIdT = runContId'

throwT :: Monad m => r -> ContT r m a
throwT = throw'

throwM :: Monad m => m r -> ContT r m a
throwM = throwM'

throwEmptyT :: Monad m => r -> ContT r m ()
throwEmptyT = throwEmpty'

throwEmptyM :: Monad m => m r -> ContT r m ()
throwEmptyM = throwEmptyM'

mapResultT :: (Monad m, Monad m') => (m r -> m' r') -> (m' r' -> m r)
  -> ContT r m a -> ContT r' m' a
mapResultT = mapResult'

catchT :: Monad m => ContT r m r -> m r
catchT = catch'

catchL :: Monad m => ContT r m r -> ContT r' m r
catchL = catchL'

catchMonoid :: (Monad m, Monoid (m r)) => ContT r m () -> m r
catchMonoid r = catchT (r >> lift mempty)

recurseT :: Monad m => (a -> ContT r m a) -> a -> ContT r m r
recurseT = recurse'

recurseFT :: Monad m => a -> (a -> ContT r m a) -> ContT r m r
recurseFT = recurseF'

loopT :: Monad m => (a -> ContT r m a) -> a -> m r
loopT = loop'

loopFT :: Monad m => a -> (a -> ContT r m a) -> m r
loopFT = loopF'

loopState :: forall m r s a. StateMonad s m => ContT r m a -> m r
loopState ra = catchT recurseState
  where
    recurseState :: ContT r m r
    recurseState = ra >> recurseState

instance Monad m => ContMonad r (ContT r m) where
  type ContRet (ContT r m) r = m r

  cm_cont :: ((a -> m r) -> m r) -> ContT r m a
  cm_cont = ContT

  cm_runCont :: ContT r m a -> (a -> m r) -> m r
  cm_runCont = _runContT

  cm_runContId :: ContT r m r -> m r
  cm_runContId m = runCont' m pure

instance Functor m => Functor (ContT r m) where
  fmap :: (a -> b) -> ContT r m a -> ContT r m b
  fmap f (ContT ra) = ContT $ \k -> ra $ k . f

instance Monad m => Applicative (ContT r m) where
  pure :: a -> ContT r m a
  pure a = ContT $ \k -> k a

  (<*>) :: ContT r m (a -> b) -> ContT r m a -> ContT r m b
  (<*>) (ContT rf) (ContT ra) =
    ContT $ \k ->
      rf $ \f ->
        ra $ k . f

instance Monad m => Monad (ContT r m) where
  (>>=) :: ContT r m a -> (a -> ContT r m b) -> ContT r m b
  (>>=) (ContT ra) f =
    ContT $ \k ->
      ra $ \a ->
        runCont' (f a) k


instance MonadT (ContT r) where
  lift :: Monad m => m a -> ContT r m a
  lift ma = ContT (ma >>=)

asContT :: Cont (m r) a -> ContT r m a
asContT (Cont ra) = ContT ra

asCont :: ContT r m a -> Cont (m r) a
asCont (ContT ra) = Cont ra

asContTNest :: (MonadERun m, Monad n) => ContT (m r) n a -> ContT r (ElevMonad m n) a
asContTNest (ContT ra) = ContT $ elevNest . ra . (runElev .)

unContTNest :: (MonadERun m, Monad n) => ContT r (ElevMonad m n) a -> ContT (m r) n a
unContTNest (ContT ra) = ContT $ runElev . ra . (elevNest .)

asContTFlip :: (MonadERun m, MonadERun n) => ContT (m r) n a -> ContT r (ElevMonad n m) a
asContTFlip (ContT ra) = ContT $ elevFlip . ra . (runElevFlip .)

unContTFlip :: (MonadERun m, MonadERun n) => ContT r (ElevMonad n m) a -> ContT (m r) n a
unContTFlip (ContT ra) = ContT $ runElevFlip . ra . (elevFlip .)
