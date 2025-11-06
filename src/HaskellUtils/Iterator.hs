{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
module HaskellUtils.Iterator where

import HaskellUtils.State
import HaskellUtils.Reader

newtype Iter a = Iter { toList :: [a] }

iterr :: Foldable f => f a -> Iter a
iterr f = Iter $ foldr (:) [] f

iterl :: Foldable f => f a -> Iter a
iterl f = Iter $ foldl (flip (:)) [] f

next :: Iter a -> (Maybe a, Iter a)
next (Iter []) = (Nothing, Iter [])
next (Iter (x:xs)) = (Just x, Iter xs)

nextState :: StateMonad (Iter a) m => m (Maybe a)
nextState = do
  (a, e) <- next <$> get
  put e; return a

peek :: Iter a -> Maybe a
peek = fst . next

peekState :: StateMonad (Iter a) m => m (Maybe a)
peekState = peek <$> get

takeIter :: Int -> Iter a -> ([a], Iter a)
takeIter n (Iter xs) = (a, Iter b)
  where (a, b) = splitAt n xs

takeIterState :: StateMonad (Iter a) m => Int -> m [a]
takeIterState n = do
  (a, e) <- takeIter n <$> get
  put e; return a

dropIter :: Int -> Iter a -> Iter a
dropIter n (Iter xs) = Iter $ drop n xs

dropIterState :: StateMonad (Iter a) m => Int -> m ()
dropIterState = modify . dropIter

instance Functor Iter where
  fmap :: (a -> b) -> Iter a -> Iter b
  fmap f = Iter . fmap f . toList

instance Foldable Iter where
  foldMap :: Monoid m => (a -> m) -> Iter a -> m
  foldMap f (Iter xs) = foldMap f xs

instance Traversable Iter where
  traverse :: Applicative f => (a -> f b) -> Iter a -> f (Iter b)
  traverse f (Iter xs) = Iter <$> traverse f xs
