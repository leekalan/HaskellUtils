{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module While where

while :: (a -> Bool) -> (a -> a) -> a -> a
while p f x = if p x then while p f (f x) else x

whileM_ :: Monad m => m Bool -> m () -> m ()
whileM_ p f = do
  b <- p
  {-# HLINT ignore "Use when" #-}
  if b then f >> whileM_ p f
  else return ()

whileM :: (Monad m, Monoid a) => m Bool -> m a -> m a
whileM p f = do
  b <- p
  if b then do
    na <- f
    na' <- whileM p f
    return $ na <> na'
  else return mempty
