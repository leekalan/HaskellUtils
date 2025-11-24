{-# LANGUAGE InstanceSigs, RankNTypes, ScopedTypeVariables #-}
module HaskellUtils.Cont where

import HaskellUtils.Transformer
import HaskellUtils.State
import Control.Applicative

type Block r = Cont r r
type BlockT m r = ContT r m r

type Seg r = Cont r ()
type SegT m r = ContT r m ()

type Scope r = Cont r
type ScopeT m r = ContT r m

type Loop r a = a -> Cont r a
type LoopT m r a = a -> ContT r m a


-- The function from (a -> r) -> r contains a state
-- which either applys the function a -> r to an a or
-- returns the already calculated r
newtype Cont r a = Cont { _runCont :: (a -> r) -> r }

cont :: ((a -> r) -> r) -> Cont r a
cont = Cont

runCont :: Cont r a -> (a -> r) -> r
runCont = _runCont

throw :: r -> forall a. Cont r a
throw r = Cont $ const r

throwEmpty :: r -> Cont r ()
throwEmpty = throw

mapResult :: (r -> r') -> (r' -> r) -> Cont r a -> Cont r' a
mapResult f g (Cont ra) = Cont $ \k -> f $ ra (g . k)

catch :: Cont r r -> r
catch rr = runCont rr id

catchM :: Cont r r -> Cont r' r
catchM rr = return $ catch rr

recurse :: (a -> Cont r a) -> a -> forall x. Cont r x
recurse f a = f a >>= recurse f

recurseF :: a -> (a -> Cont r a) -> forall x. Cont r x
recurseF = flip recurse

loop :: (a -> Cont r a) -> a -> r
loop f a = catch $ recurse f a

loopF :: a -> (a -> Cont r a) -> r
loopF = flip loop

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
        runCont (f a) k -- 'f a' gives us 'Cont r b' for which k is passed
                        -- this gives us an r


newtype ContT r m a = ContT { _runContT :: (a -> m r) -> m r }

contT :: ((a -> m r) -> m r) -> ContT r m a
contT = ContT

runContT :: ContT r m a -> (a -> m r) -> m r
runContT = _runContT

throwT :: Applicative m => r -> ContT r m a
throwT r = ContT $ const $ pure r

throwM :: m r -> ContT r m a
throwM mr = ContT $ const mr

throwEmptyT :: Applicative m => r -> ContT r m ()
throwEmptyT = throwT

throwEmptyM :: m r -> ContT r m ()
throwEmptyM = throwM

mapResultT :: (m r -> m' r') -> (m' r' -> m r) -> ContT r m a -> ContT r' m' a
mapResultT f g (ContT ra) = ContT $ \k -> f $ ra (g . k)

catchT :: Applicative m => ContT r m r -> m r
catchT rr = runContT rr pure

catchL :: Monad m => ContT r m r -> ContT r' m r
catchL = lift . catchT

catchAlternative :: (Monad m, Alternative m) => ContT r m () -> m r
catchAlternative r = catchT (r >> lift empty)

recurseT :: (a -> ContT r m a) -> a -> ContT r m r
recurseT f a = f a >>= recurseT f

recurseFT :: a -> (a -> ContT r m a) -> ContT r m r
recurseFT = flip recurseT

loopT :: (a -> ContT r m a) -> a -> m r
loopT = loopT

loopFT :: a -> (a -> ContT r m a) -> m r
loopFT = loopFT

-- loopState :: forall m r s a. StateMonad s m => ContT r m a -> m r
-- loopState ra = catchT recurseState
--   where
--     recurseState :: ContT r m r
--     recurseState = ra >> recurseState

instance Functor (ContT r m) where
  fmap :: (a -> b) -> ContT r m a -> ContT r m b
  fmap f (ContT ra) = ContT $ \k -> ra $ k . f

instance Applicative (ContT r m) where
  pure :: a -> ContT r m a
  pure a = ContT $ \k -> k a

  (<*>) :: ContT r m (a -> b) -> ContT r m a -> ContT r m b
  (<*>) (ContT rf) (ContT ra) =
    ContT $ \k ->
      rf $ \f ->
        ra $ k . f

instance Monad (ContT r m) where
  (>>=) :: ContT r m a -> (a -> ContT r m b) -> ContT r m b
  (>>=) (ContT ra) f =
    ContT $ \k ->
      ra $ \a ->
        runContT (f a) k


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
