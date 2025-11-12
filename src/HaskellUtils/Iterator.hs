{-# LANGUAGE InstanceSigs, FlexibleContexts #-}
module HaskellUtils.Iterator where

import Data.Foldable
import Data.Bifunctor

newtype Iter a = Iter { next :: (a, Iter a) }

iterList :: [a] -> Iter (Maybe a)
iterList [] = Iter (Nothing, iterList [])
iterList (x:xs) = Iter (Just x, iterList xs)

fromFoldable :: Foldable f => f a -> Iter (Maybe a)
fromFoldable = f . toList
  where
    f [] = Iter (Nothing, f [])
    f (x:xs) = Iter (Just x, f xs)

-- nextState :: StateMonad (Iter a) m => m (Maybe a)
-- nextState = do
--   (a, e) <- next <$> get
--   put e; return a

-- peek :: Iter a -> Maybe a
-- peek = fst . next

-- peekState :: StateMonad (Iter a) m => m (Maybe a)
-- peekState = peek <$> get

-- takeIter :: Int -> Iter a -> ([a], Iter a)
-- takeIter n (Iter xs) = (a, Iter b)
--   where (a, b) = splitAt n xs

-- takeIterState :: StateMonad (Iter a) m => Int -> m [a]
-- takeIterState n = do
--   (a, e) <- takeIter n <$> get
--   put e; return a

-- dropIter :: Int -> Iter a -> Iter a
-- dropIter n (Iter xs) = Iter $ drop n xs

-- dropIterState :: StateMonad (Iter a) m => Int -> m ()
-- dropIterState = modify . dropIter

instance Functor Iter where
  fmap :: (a -> b) -> Iter a -> Iter b
  fmap f (Iter (a, as)) = Iter (f a, fmap f as)

instance Foldable Iter where
  foldr :: (a -> b -> b) -> b -> Iter a -> b
  foldr f b (Iter (a, as)) = f a $ foldr f b as

instance Traversable Iter where
  traverse :: Applicative f => (a -> f b) -> Iter a -> f (Iter b)
  traverse f (Iter (a, as)) = fmap (curry Iter) (f a) <*> traverse f as

newtype IterT m a = IterT { nextT :: m (a, IterT m a) }

instance Functor m => Functor (IterT m) where
  fmap :: (a -> b) -> IterT m a -> IterT m b
  fmap f (IterT ma) = IterT $ fmap (bimap f (fmap f)) ma

-- instance Monad m => Foldable (IterT m) where
--   foldr :: Monad m => (a -> b -> b) -> b -> IterT m a -> b
--   foldr f b (IterT ma) = fmap (\(a, as) -> f a $ foldr f b as) ma
  
