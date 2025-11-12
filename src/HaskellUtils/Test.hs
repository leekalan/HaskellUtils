{-# LANGUAGE FlexibleContexts #-}
module HaskellUtils.Test where

import HaskellUtils.State
import HaskellUtils.Reader
import HaskellUtils.Transformer
import HaskellUtils.Cont
import Control.Monad

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
    vp <- if v < 0 then throwAny' "negative" else return v
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

--        Loop  Return    Inc
--         |      |        |
--         v      v        v
find63 :: LoopT String IO Int
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
