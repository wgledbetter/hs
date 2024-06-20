-- Concurrency
{-# LANGUAGE NumericUnderscores #-}

module PCP.Ch07 where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Foldable
import System.IO
import System.Random
import Text.Printf

-- Threads and MVars -----------------------------------------------------------

main1 :: IO ()
main1 = do
  hSetBuffering stdout NoBuffering
  forkIO (replicateM_ 1000 (putChar 'A'))
  replicateM_ 1000 (putChar 'B')

main2 :: IO ()
main2 = loop
  where
    loop = do
      s <- getLine
      if s == "exit"
        then return ()
        else do
          forkIO $ setReminder s
          loop

setReminder :: String -> IO ()
setReminder s = do
  printf "Reminder in %d seconds.\n" t
  threadDelay (10 ^ 6 * t)
  printf "Your %d seconds is up.\BEL\n" t
  where
    t = read s :: Int

-- Communication: MVars --------------------------------------------------------

-- MVars can be "empty" or "full".
-- This means they're only good for passing one value at a time.
-- In the Argus context though, this makes it really clear when something has been consumed.

-- This does weird things when run with "+RTS -N(>2)"
main3 :: IO ()
main3 = do
  m <- newEmptyMVar
  forkIO
    ( do
        v <- takeMVar m
        printf "First thread got an MVar: %s\n" v
    )
  forkIO
    ( do
        v <- takeMVar m
        printf "Second thread got an MVar: %s\n" v
    )
  forkIO
    ( do
        v <- takeMVar m
        printf "Third thread got an MVar: %s\n" v
    )
  -- v1 <- getLine
  putMVar m "heyo"
  -- v2 <- getLine
  putMVar m "wow"

-- Exception "BlockedIndefinitelyOnMVar" can crash hanging programs.
-- This is good because then you know if something is dead or just slow.

-- MVars _are_ effictively atomic.
-- But reading some other documentation, we have "TVar" and "atomically".

main4 :: IO ()
main4 = do
  t <- atomically $ newTVar "a"
  forkIO
    ( do
        v <- atomically $ readTVar t
        printf "1: %d\n" v
    )
  forkIO
    ( do
        v <- atomically $ readTVar t
        printf "2: %d\n" v
    )
  forkIO
    ( do
        v <- atomically $ readTVar t
        printf "3: %d\n" v
    )
  atomically $ writeTVar t "b"
  threadDelay (10 ^ 6 * 1)
  atomically $ writeTVar t "c"

-- Yeah, all threads can read the same value.
-- Supposedly, TVars are "safer but slower" (https://stackoverflow.com/a/15440209)

data Node a = Source (TVar a) | Computation (STM a)

getMyValue :: Node a -> STM a
getMyValue (Source tv) = readTVar tv
getMyValue (Computation stm) = stm

instance (Num a) => Num (Node a) where
  mvx + mvy = Computation (liftA2 (+) (getMyValue mvx) (getMyValue mvy))
  mvx * mvy = Computation (liftA2 (*) (getMyValue mvx) (getMyValue mvy))
  abs mv = Computation $ fmap abs $ getMyValue mv
  signum mv = Computation $ fmap signum $ getMyValue mv
  negate mv = Computation $ fmap negate $ getMyValue mv
  fromInteger i = Computation $ return (fromInteger i)

someEquation :: (Num a) => Node a -> Node a -> Node a
someEquation x y = y - abs x

newValue :: a -> IO (Node a)
newValue x = atomically $ Source <$> newTVar x

setValue :: Node a -> a -> STM ()
setValue (Source tv) a = writeTVar tv a

timesDo :: (Monoid (m a), Monad m) => Int -> m a -> m a
n `timesDo` m = fold $ replicate n m

testMyValue :: IO ()
testMyValue = do
  -- Just print a value
  x <- newValue (-1.5)
  v <- atomically $ getMyValue x
  print v
  -- Perform a computation on a value
  let y = abs x
  w <- atomically $ getMyValue y
  print w
  -- Perform a different computation on a value
  a <- atomically $ getMyValue (x + y)
  print a
  -- Perform a computation on two values that change over time
  z <- newValue 4.7
  forkIO
    ( 5 `timesDo` do
        out <- atomically $ getMyValue $ someEquation x z
        print out
        threadDelay (1000) -- Microseconds
    )
  atomically $ setValue z 5.3
  10 `timesDo` do
    threadDelay 750
    rd <- randomRIO (-10, 10) :: IO Double
    rd' <- randomRIO (1, 2) :: IO Double
    atomically $ (setValue z rd >> setValue x rd')

-- Okay, excellent. This does what I was hoping I could do, but I'm not sure I'm doing it the intended way.
-- This stuff is actually in Ch10, so I might skip on to that.
-- Questions / Next Steps:
-- - Should I be wrapping my entire computations in STM? Or should STM only be used for pulling values from TVars?
--   - I'd guess it's the latter. That way you minimize the footprint of these atomic operations.
-- - Is there a way to write this that isn't polluted with STM or IO the whole way?
--   - I want a datatype that can hold sources as TVars or MVars but also do the composable operations.

-- TVars always for data, MVars sometimes for notify?
-- Each source keeps a list of MVars that it pushes to when it gets new data.
--   But then we have the same problem with fast sources where their
--   Is there a way for the sink to immediately pull the MVar notification signal?
--   Maybe something like (pseudo-code): "concurrentlyWaitFor [do _ <- takeMVar m | m <- allSourceMVars]"
--   The "concurrentlyWaitFor" function would spawn each "takeMVar m" on a different thread, and block until
--   We might actually need something like "concurrentlyBlockUntilEachCompletesAtLeastOnce"
--   I really only see this working like the Argus C++ notify queue

-- Equivalent to LazyMode::Everything
-- bothAreReady :: MVar Bool -> MVar Bool -> IO ()
-- bothAreReady m1 m2 = do
--   ready <- atomically $ do
--     proceed <- all ts
--     if proceed then return proceed else retry
--   return ready
--   where
--     ms :: [MVar Bool]
--     ms = [m1, m2]
--     -- Create TVars to hold consumed MVars
--     ts :: STM [TVar Bool]
--     ts = map (const (atomically $ newTVar False)) ms

blockUntilReady :: [TVar Bool] -> STM ()
blockUntilReady tbs = do
  bs <- mapM readTVar tbs
  if all (== True) bs then return () else retry

-- This is really crucial, _and_ it worked the first time after ironing out compiler issues!
blockedCallback :: [TVar Bool] -> IO () -> IO ()
blockedCallback conditions action = forever $ do
  atomically $
    do
      blockUntilReady conditions
      clearStatus conditions
  action

clearStatus :: [TVar Bool] -> STM ()
clearStatus conditions = traverse (flip writeTVar False) conditions >> return ()

consumeMVar :: MVar a -> TVar a -> IO ThreadId
consumeMVar mb tb = forkIO $ forever $ do
  v <- takeMVar mb
  atomically $ writeTVar tb v

fakeAction :: Int -> IO ()
fakeAction i = printf "We actin: %d\n" i

testConsume :: IO ()
testConsume = do
  m <- newEmptyMVar :: IO (MVar Bool)
  t <- atomically $ newTVar False
  consumeMVar m t
  tv1 <- atomically $ readTVar t
  print tv1
  threadDelay 1000
  tv2 <- atomically $ readTVar t
  print tv2
  putMVar m True
  threadDelay 1000
  tv3 <- atomically $ readTVar t
  print tv3

testCallback :: IO ()
testCallback = do
  t1 <- atomically $ newTVar False
  t2 <- atomically $ newTVar False
  forkIO $ blockedCallback [t1, t2] (fakeAction 0)
  print "Setting t1 True"
  atomically $ writeTVar t1 True
  threadDelay 50000
  print "Setting t2 True"
  atomically $ writeTVar t2 True
  threadDelay 50000
  print "Setting t2 True"
  atomically $ writeTVar t2 True
  print "Setting t2 False"
  atomically $ writeTVar t2 False
  threadDelay 25000
  print "Setting t1 True"
  atomically $ writeTVar t1 True
  print "Setting t2 True"
  atomically $ writeTVar t2 True
  threadDelay 50000
  print "Press enter to quit"
  _ <- getLine
  return ()

periodicNullSource :: Int -> [TVar Bool] -> IO ()
periodicNullSource period conditions = forever $ do
  threadDelay period
  atomically $ traverse (\tv -> writeTVar tv True) conditions

testCallback2 :: IO ()
testCallback2 = do
  -- Create enough condition variables for the scenario
  t1 <- atomically $ newTVar False
  t2 <- atomically $ newTVar False
  t3 <- atomically $ newTVar False

  -- Connection Map:
  -- - t1 connects source 1 to sink 1
  -- - t2 connects source 1 to sink 2
  -- - t3 connects source 2 to sink 2

  -- Start sources
  forkIO $ periodicNullSource 100_000 [t1, t2]
  forkIO $ periodicNullSource 500_000 [t3]

  -- Start sinks
  forkIO $ blockedCallback [t1] (fakeAction 0)
  forkIO $ blockedCallback [t2, t3] (fakeAction 1)

  -- Stall while it runs
  threadDelay 5_000_000
