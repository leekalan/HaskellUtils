{-# LANGUAGE FlexibleContexts, TypeApplications #-}
module HaskellUtils.Test where

import HaskellUtils.State
import HaskellUtils.Reader
import HaskellUtils.Transformer

incrementRead :: MonadT r => (ReaderMonad Int (r s), StateMonad Int s) => r s ()
incrementRead = do
  r <- get
  lift $ modify (+r)

incrementWith :: StateMonad Int m => Int -> m ()
incrementWith = runReader @(ReaderT _ _) incrementRead

main :: IO ()
main = do
  let x = evalState @(State _) (incrementWith 3) 2
  print x
