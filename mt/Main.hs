-- Learning parallelism: https://wiki.haskell.org/Parallel/Reading

module Main where

import Control.Parallel.Strategies
import MT.HFM
import MT.Repa
import Memo
import PCP.Ch03

-- PCP Ch03 Tests --------------------------------------------------------------

-- Even though we only take 10, the parMap call over the longer list causes the runtime to be as if we calculated everything in the list.
-- Ah, that's just for my handwritten functions. The library implementations are solid. Don't work for infinite lists, but don't pessimize calculating unused list items.
pcpCh03_a :: IO ()
pcpCh03_a = do
  let mm = map slowFib [1 .. 1000] `using` parList rpar
  print $ take 10 mm

pcpCh03_b :: IO ()
pcpCh03_b = do
  let mm = map slowFib [1 .. 1000]
  print $ take 10 mm

-- Main ------------------------------------------------------------------------

-- NOTE: run as "stack run -- mt +RTS -N666"
main :: IO ()
main = pcpCh03_a
