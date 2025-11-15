{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
module HaskellUtils.Test where

import HaskellUtils.State
import HaskellUtils.Reader
import HaskellUtils.Transformer
import HaskellUtils.Cont
import HaskellUtils.DelimCont
import HaskellUtils.Maybe

import Control.Monad
import HaskellUtils.Any

incrementRead :: MonadT r => (ReaderMonad Int (r s), StateMonad Int s) => r s ()
incrementRead = do
  r <- ask'
  lift $ modify' (+r)

incrementWith :: StateMonad Int s => Int -> s ()
incrementWith = runReaderT incrementRead

testRead :: IO ()
testRead = do
  let x = evalState (incrementWith 3)
  print $ x 2


testCont :: Int -> IO ()
testCont v = do
  print $ catch $ do
    vp <- if v < 0 then throw' "negative" else return v
    return ("postive " ++ show vp)


test :: Int -> BlockT (Scope String) Int
test n = do
  lift $ when (n < 0) $ throw' "error"
  return 10

test2 :: Int -> Block String
test2 n = do
  x <- catch' $ test n
  return $ show x


--              Loop Return Inc
--               |     |     |
--               v     v     v
find63Simple :: Loop String Int
find63Simple n = do
  when (n == 63) $ throw' "found it!"
  -- incrementing counter
  return $ n + 1

--        Loop     Return Inc
--         |         |     |
--         v         v     v
find63 :: LoopT IO String Int
find63 n = do
  -- inner scope that catches the throw
  string <- catchL $ do
    when (n == 63) $ throw' "found it!"

    let ret = "not it: " ++ show n
    lift $ print ret
    return ret

  -- exits loop if 'found it!'
  when (string == "found it!") $ throw' "found it!"

  -- incrementing counter
  return $ n + 1

testFind63 :: Int -> IO ()
testFind63 n = do
  print =<< loopT find63 n


data Tree = Leaf (Int, String) | Branch Tree Tree

treeFind :: Int -> Tree -> MaybeT IO String
treeFind n t = runDelimThrowMemptyT $ search t
-- 
-- It could also be the following when done manually:
--   treeFind n t = catchT $ runDelimThrowMT (search t) $ const Nothing
-- 
-- Or if you really want to to it manually:
--   treeFind n t =
--     let throwNothing = const $ throwT Nothing
--     in  catchT $ asContTNest $ runDelimT (search t) throwNothing
-- 
  where
    search :: Tree -> DelimSegT IO String
    search (Leaf (x, s)) = when (x == n) $ throwDelimT s
    search (Branch l r) = do
      lift $ print "branch!"
      search l >> search r


printAny :: [BoundAny Show] -> IO ()
printAny = mapM_ $ \a -> runBoundAny a print

printAnyIO :: [BoundAnyF Show IO] -> IO ()
printAnyIO = mapM_ $ \a -> runBoundAnyF a (>>= print)

printAnyIOTest :: IO ()
printAnyIOTest = do
  let arr = [
          boundAnyF $ do print "first" >> return (10 :: Integer),
          boundAnyF $ do print "second" >> return "text"
        ]
  printAnyIO arr
