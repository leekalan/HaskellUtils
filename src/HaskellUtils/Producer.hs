{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE InstanceSigs #-}
module HaskellUtils.Producer where

import HaskellUtils.PartialCont
import Data.Kind
import HaskellUtils.Transformer
import Control.Applicative

class Producer p where
  type ProducerResult p :: Type -> Type

  produce :: p a -> (ProducerResult p a, p a)


newtype EmptyProducer (m :: Type -> Type) (p :: Type -> Type) a =
  EmptyProducer { unMaybeProducer :: p a }

asEmptyProducer :: p a -> EmptyProducer m p a
asEmptyProducer = EmptyProducer

instance (Alternative m, Producer p, ProducerResult p ~ ParCont x)
  => Producer (EmptyProducer m p) where
  type ProducerResult (EmptyProducer m p) = m

  produce :: EmptyProducer m p a -> (m a, EmptyProducer m p a)
  produce (EmptyProducer p) = 
    let (pc, p') = produce p
    in (runParEmpty pc, EmptyProducer p')

type MaybeProducer = EmptyProducer Maybe

asMaybeProducer :: p a -> MaybeProducer p a
asMaybeProducer = asEmptyProducer


newtype EmptyProducerT (n :: Type -> Type) (p :: Type -> Type) (m :: Type -> Type) a = EmptyProducerT { unEmptyProducerT :: p a }

asEmptyProducerT :: p a -> EmptyProducerT n p m a
asEmptyProducerT = EmptyProducerT

instance (Monad m, MonadERun n, Alternative n, Producer p, ProducerResult p ~ ParContT x m)
  => Producer (EmptyProducerT n p m) where
  type ProducerResult (EmptyProducerT n p m) = (ElevMonad n m)

  produce :: EmptyProducerT n p m a -> ((ElevMonad n m) a, EmptyProducerT n p m a)
  produce (EmptyProducerT p) =
    let (pc, p') = produce p
    in (runParEmptyT pc, EmptyProducerT p')

type MaybeProducerT = EmptyProducerT Maybe

asMaybeProducerT :: p a -> MaybeProducerT p m a
asMaybeProducerT = asEmptyProducerT
