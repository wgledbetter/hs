module PCP.Ch10 where

import Control.Concurrent.STM

-- All actions inside an STM are performed atomically, not just the reads and writes.
-- This is a "transaction."
-- "Scalable Atomicity"

-- Blocking --------------------------------------------------------------------

newtype MyMVar a = MyMVar (TVar (Maybe a))

newEmptyMyMVar :: STM (MyMVar a)
newEmptyMyMVar = do
  x <- newTVar Nothing
  return (MyMVar x)

takeMyMVar :: MyMVar a -> STM a
takeMyMVar (MyMVar t) = do
  m <- readTVar t
  case m of
    Nothing -> retry -- Block until it has contents
    Just x -> do
      writeTVar t Nothing
      return x

putMyMVar :: MyMVar a -> a -> STM ()
putMyMVar (MyMVar t) v = do
  m <- readTVar t
  case m of
    Just _ -> retry -- Block until it has space for new value
    Nothing ->
      writeTVar t (Just v)

-- "taking a single MVar is a side effect that is visible to the rest of the program,
--  and hence cannot be easily undone if the other MVar is empty"

-- Merging ---------------------------------------------------------------------

-- orElse :: STM a -> STM a -> STM a
-- orElse x y
-- Do x
-- If it succeeds, return it
-- If it retries, drop it and do y

-- Note that this whole function is an STM, so it'll happen atomically.
takeEitherMyMVar :: MyMVar a -> MyMVar b -> STM (Either a b)
takeEitherMyMVar x y = fmap Left (takeMyMVar x) `orElse` fmap Right (takeMyMVar y)

-- Performance -----------------------------------------------------------------

-- Rules:
-- 1. Never read an unbounded number of TVars in a single transaction.
-- 2. Avoid expensive evaluation inside a transaction.
-- 3. Avoid composition of blocking functions. Split when possible.
