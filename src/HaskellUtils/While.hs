module HaskellUtils.While where

import Control.Monad ( when )

while :: (a -> Bool) -> (a -> a) -> a -> a
while p f x = if p x then while p f (f x) else x

whileM_ :: Monad m => m Bool -> m () -> m ()
whileM_ p f = do
  b <- p
  when b $ f >> whileM_ p f

whileM :: (Monad m, Monoid a) => m Bool -> m a -> m a
whileM p f = do
  b <- p
  if b then do
    na <- f
    na' <- whileM p f
    return $ na <> na'
  else return mempty

loopUntil :: (a -> Either a b) -> a -> b
loopUntil f x = case f x of
  Left a -> loopUntil f a
  Right b -> b

loopUntilM_ :: Monad m => m (Maybe a) -> m a
loopUntilM_ f = do
  x <- f
  case x of
    Just a -> return a
    Nothing -> loopUntilM_ f

loopUntilM :: Monad m => (a -> m (Either a b)) -> a -> m b
loopUntilM f g = do
  x <- f g
  case x of
    Left a -> loopUntilM f a
    Right b -> return b
