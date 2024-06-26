module Core where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Foldable
import Data.Time
import Text.Printf

-- Types -----------------------------------------------------------------------

data Node a = Source (TVar a) | Computation (IO a)

getNodeValue :: Node a -> IO a
getNodeValue (Source tv) = readTVarIO tv
getNodeValue (Computation io) = io

-- Sources ---------------------------------------------------------------------

timeSource :: Int -> TVar UTCTime -> [TVar Bool] -> IO ThreadId
timeSource period v conditions = forkIO $ forever $ do
  threadDelay period
  val <- getCurrentTime
  atomically $ do
    writeTVar v val
    traverse (`writeTVar` True) conditions

-- Sinks -----------------------------------------------------------------------

blockUntilAllReady :: [TVar Bool] -> STM ()
blockUntilAllReady tbs = do
  bs <- mapM readTVar tbs
  unless (and bs) retry

blockUntilOneReady :: [TVar Bool] -> STM ()
blockUntilOneReady tbs = do
  bs <- mapM readTVar tbs
  unless (or bs) retry

clearStatus :: [TVar Bool] -> STM ()
clearStatus = traverse_ (`writeTVar` False)

blockedCallback :: [TVar Bool] -> IO () -> IO ThreadId
blockedCallback conditions action = forkIO $ forever $ do
  atomically $
    do
      blockUntilAllReady conditions
      clearStatus conditions
  action

timedCallback :: Int -> IO () -> IO ThreadId
timedCallback delay action = forkIO $ forever $ do
  threadDelay delay
  action

-- Actions ---------------------------------------------------------------------

printAction :: (PrintfArg a) => Int -> Node a -> IO ()
printAction id node = do
  nVal <- getNodeValue node
  printf "printAction ID (%d): Value = %d\n" id nVal
