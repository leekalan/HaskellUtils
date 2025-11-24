{-# LANGUAGE FlexibleContexts, RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
module HaskellUtils.Queue where

import HaskellUtils.DelimCont
import HaskellUtils.PartialCont
import HaskellUtils.State
import Control.Arrow
import HaskellUtils.Producer

type EmptyQueue = EmptyProducer Queue
type EmptyTQueue = EmptyProducerT Queue
type MaybeQueue = MaybeProducer Queue
type MaybeTQueue = MaybeProducerT Queue


-- class Queue p where
--   type QueueResult p :: Type -> Type

--   enqueue :: a -> p a -> p a
--   dequeue :: p a -> (QueueResult p a, p a)


data Queue a = Q [a] [a]

newQueue :: Queue a
newQueue = Q [] []

newQueueList :: [a] -> Queue a
newQueueList fs = Q fs []

enqueue :: a -> Queue a -> Queue a
enqueue a (Q fs bs) = Q fs (a:bs)

dequeue :: Queue a -> (ParSeg a, Queue a)
dequeue q = first delimAsPar $ dequeueDelim q
  where
    dequeueDelim :: Queue a -> (DelimSeg a, Queue a)
    dequeueDelim (Q (a:as) bs) = (throwDelim a, Q as bs)
    dequeueDelim (Q [] []) = (return (), Q [] [])
    dequeueDelim (Q [] bs) = dequeueDelim (Q (reverse bs) [])

drainQueue :: Queue a -> [a]
drainQueue (Q as bs) = as ++ reverse bs

-- enqueueState' :: forall m a. StateMonad (Queue a) m => a -> m ()
-- enqueueState' = modify' . enqueue

-- enqueueState :: a -> State (Queue a) ()
-- enqueueState = enqueueState'

-- enqueueStateT :: Monad m => a -> StateT (Queue a) m ()
-- enqueueStateT = enqueueState'

-- dequeueState' :: forall m a. StateMonad (Queue a) m => m (ParSeg a)
-- dequeueState' = do
--   (a, q) <- fmap dequeue get'
--   put' q; return a

-- dequeueState :: State (Queue a) (ParSeg a)
-- dequeueState = dequeueState'

-- dequeueStateT :: Monad m => StateT (Queue a) m (ParSeg a)
-- dequeueStateT = dequeueState'

-- drainQueueState' :: forall m a. StateMonad (Queue a) m => m [a]
-- drainQueueState' = do
--   q <- get'
--   put' newQueue
--   return $ drainQueue q

-- drainQueueState :: State (Queue a) [a]
-- drainQueueState = drainQueueState'

-- drainQueueStateT :: Monad m => StateT (Queue a) m [a]
-- drainQueueStateT = drainQueueState'

instance Producer Queue where
  type ProducerResult Queue = ParSeg
  produce :: Queue a -> (ParSeg a, Queue a)
  produce = dequeue
