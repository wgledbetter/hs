module PCP.BenchCh10 where

import Control.Concurrent
import Control.Concurrent.STM
import Criterion.Main

-- Full bench group ------------------------------------------------------------

benchPCP10 = bgroup "PCP/Ch10" [benchMVar, benchTVar1, benchTVar2]

-- Individual tests ------------------------------------------------------------

benchMVar = bench "Create, write, and read MVar" $ whnf go 12
  where
    go v = do
      m <- newEmptyMVar :: IO (MVar Int)
      putMVar m v
      x <- takeMVar m
      return ()

-- Individual atomic actions
benchTVar1 = bench "Create, write, and read TVar (case 1)" $ whnf go 12
  where
    go v = do
      t <- newTVarIO 0
      atomically $ writeTVar t v
      x <- atomically $ readTVar t
      return ()

-- Single composed atomic action
benchTVar2 = bench "Create, write, and read TVar (case 2)" $ whnf go 12
  where
    go v = do
      atomically $ do
        t <- newTVar 0
        writeTVar t v
        x <- readTVar t
        return ()

-- Conclusion: Yes, MVar is actually faster than TVar, but it's like a half a
--             nanosecond, and the percentage is like 12.5%. The gap also seems
--             to stay relatively constant with increased threads, though all do
--             slow down.
