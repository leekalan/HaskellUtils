{-# LANGUAGE FlexibleContexts, MonoLocalBinds #-}
{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
module HaskellUtils.Test where

import HaskellUtils.State
import HaskellUtils.Reader
import HaskellUtils.Transformer
import HaskellUtils.Cont
import HaskellUtils.Maybe
import HaskellUtils.DelimCont
import HaskellUtils.Any
import HaskellUtils.Queue
import HaskellUtils.PartialCont

import Control.Monad

testLiftReader :: MaybeT (MaybeT (ReaderT Int IO)) ()
testLiftReader = liftR readerThing

readerNoLift :: Reader () ()
readerNoLift = liftR readerNoLift

testLiftReaderNest :: MaybeT (ReaderT Int (MaybeT (ReaderT Int IO))) ()
testLiftReaderNest = do
  liftR readerThing
  liftR readerThingUpper
  liftR ioThing

readerThing :: ReaderT Int IO ()
readerThing = elev nop

readerThingUpper :: ReaderT Int (MaybeT (ReaderT Int IO)) ()
readerThingUpper = elev nop

ioThing :: IO ()
ioThing = nop

simpleRead :: forall rt m. (ReaderMonad Int (LiftTrans rt m), LiftMonadT m rt)
  => LiftTrans rt m Int
simpleRead = ask

simpleReadWith :: forall m. (Monad m, LiftMonadT m (ReaderT Int)) => Int -> m Int
simpleReadWith = runReaderT $ simpleRead @(ReaderT Int) @m

testSimpleRead :: Int -> IO ()
testSimpleRead v = do
  x <- simpleReadWith @IO v
  print x

incrementRead :: forall mt rt s.
  LiftComposed mt rt s
  => (ReaderMonad Int (LiftTrans rt s), StateMonad Int s)
  => LiftTrans2 mt rt s ()
incrementRead = do
  r <- liftT @(LiftTrans rt s) @mt $ ask
  liftT @s @(ComposeT mt rt) $ modify (+r)

incrementWith :: forall mt s. StateMonad Int s
  => LiftMonadT s mt
  => Int -> LiftTrans mt s ()
incrementWith = runReaderT $ incrementRead @IdentityLift @(ComposeT (ReaderT _) mt) @s

testRead :: IO ()
testRead = do
  let x = evalState $ catchT $ incrementWith @(ContT ()) @(State Int) 3
  print $ x 2


testCont :: Int -> IO ()
testCont v = do
  print $ catch $ do
    vp <- if v < 0 then throw "negative" else return v
    return ("postive " ++ show vp)


test :: Int -> BlockT (Scope String) Int
test n = do
  lift $ when (n < 0) $ throw "error"
  return 10

test2 :: Int -> Block String
test2 n = do
  x <- catchT $ test n
  return $ show x


--              Loop Return Inc
--               |     |     |
--               v     v     v
find63Simple :: Loop String Int
find63Simple n = do
  when (n == 63) $ throw "found it!"
  -- incrementing counter
  return $ n + 1

--        Loop     Return Inc
--         |         |     |
--         v         v     v
find63 :: LoopT IO String Int
find63 n = do
  -- inner scope that catches the throw
  string <- catchL $ do
    when (n == 63) $ throwT "found it!"

    let ret = "not it: " ++ show n
    lift $ print ret
    return ret

  -- exits loop if 'found it!'
  when (string == "found it!") $ throwT "found it!"

  -- incrementing counter
  return $ n + 1

testFind63 :: Int -> IO ()
testFind63 n = do
  print =<< loopT find63 n


data Tree = Leaf (Int, String) | Branch Tree Tree

treeFind :: Int -> Tree -> MaybeT IO String
treeFind n t = runDelimThrowEmptyT $ search t
-- 
-- It could also be the following when done manually:
--   treeFind n t = catchT $ runDelimThrowMT (search t) $ const Nothing
-- 
-- Or if you really want to to it manually:
--   treeFind n t =
--     let nothing = const $ return Nothing
--     in  catchT $ asContTNest $ runDelimThrowT (search t) nothing
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


queueTest :: [Int] -> IO ()
queueTest xs = do
  void $ runStateTF newQueue $ do
    elev $ fillQueue xs
    printQueue
  where
    fillQueue :: [Int] -> State (Queue Int) ()
    fillQueue (y:ys) = do
      enqueueState y
      fillQueue ys
    fillQueue [] = nop

    printQueue :: StateT (Queue Int) IO ()
    printQueue = do
      ps <- dequeueStateT
      let x = runParEmpty ps
      case x of
        Just y -> do
          liftR $ print y
          printQueue
        Nothing -> nop

