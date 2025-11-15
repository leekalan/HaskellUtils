{-# LANGUAGE RankNTypes #-}
module HaskellUtils.Any where

newtype Any r = Any { runAny :: (forall a. a -> r) -> r}

any' :: a -> Any r
any' a = Any ($ a)

newtype AnyF f r = AnyF { runAnyF :: (forall a. f a -> r) -> r}

anyF :: f a -> AnyF f r
anyF fa = AnyF ($ fa)


newtype Bound c r = Bound { runBound :: (forall a. c a => a -> r) -> r }

bound :: c a => a -> Bound c r
bound a = Bound ($ a)

newtype BoundF c r f = BoundF { runBoundF :: (forall a. c a => f a -> r) -> r }

boundF :: c a => f a -> BoundF c r f
boundF fa = BoundF ($ fa)


newtype AnyAny = AnyAny { _runAnyAny :: forall a. Any a }

anyAny :: a -> AnyAny
anyAny a = AnyAny $ any' a

runAnyAny :: AnyAny -> forall x. (forall a. a -> x) -> x
runAnyAny (AnyAny a) = runAny a

newtype AnyAnyF f = AnyAnyF { _runAnyAnyF :: forall a. AnyF f a }

anyAnyF :: f a -> AnyAnyF f
anyAnyF fa = AnyAnyF $ anyF fa

runAnyAnyF :: AnyAnyF f -> forall x. (forall a. f a -> x) -> x
runAnyAnyF (AnyAnyF a) = runAnyF a


newtype BoundAny c = BoundAny { _runBoundAny :: forall a. Bound c a }

boundAny :: c a => a -> BoundAny c
boundAny a = BoundAny $ bound a

runBoundAny :: BoundAny c -> forall x. (forall a. c a => a -> x) -> x
runBoundAny (BoundAny b) = runBound b

newtype BoundAnyF c f = BoundAnyF { _runBoundAnyF :: forall a. BoundF c a f }

boundAnyF :: c a => f a -> BoundAnyF c f
boundAnyF fa = BoundAnyF $ boundF fa

runBoundAnyF :: BoundAnyF c f -> forall x. (forall a. c a => f a -> x) -> x
runBoundAnyF (BoundAnyF b) = runBoundF b
