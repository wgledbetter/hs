{-# LANGUAGE NumericUnderscores #-}

module Argus.Scratch where

import Argus.Core
import Control.Concurrent
import Control.Concurrent.STM
import Data.Time

-- Utils -----------------------------------------------------------------------

getPicoseconds :: Node UTCTime -> Node Integer
getPicoseconds utc = Computation $ do
  val <- getNodeValueSTM utc
  return $ diffTimeToPicoseconds $ utctDayTime val

-- Tests -----------------------------------------------------------------------

testEagerSink1 :: IO ()
testEagerSink1 = do
  -- Create the value that holds data
  value <- newTVarIO (UTCTime (ModifiedJulianDay 0) 0)

  -- Create a source to feed the data value
  timeSource 250_000 value []

  -- Create a sink to consume the data value
  timedCallback 50_000 (printAction 0 (getPicoseconds $ Source value))

  -- Wait a bit to watch things happen
  threadDelay 5_000_000

testLazySink1 :: IO ()
testLazySink1 = do
  -- Create the value that holds data
  value <- newTVarIO (UTCTime (ModifiedJulianDay 0) 0)

  -- Create a signal value
  cond <- newTVarIO False

  -- Create a source to feed the data value
  timeSource 250_000 value [cond]

  -- Create a sink to consume the data value
  blockedCallback [cond] (printAction 0 (getPicoseconds $ Source value))

  -- Wait a bit to watch things happen
  threadDelay 5_000_000
