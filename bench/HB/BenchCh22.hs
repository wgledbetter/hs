-- Benchmarks for Num, Fractional, and Floating instances of my Reader type

module HB.BenchCh22 where

import Criterion.Main
import HB.Ch22 (Reader (Reader), runReader)

-- Full Bench Group ------------------------------------------------------------

benchCh22 =
  bgroup
    "HB/Ch22"
    [ benchRefFunc1,
      benchRdrFunc1
    ]

-- Individual Tests ------------------------------------------------------------

-- Scenario 1: Explicit vs. Reader

refFunc1 :: Double -> Double
refFunc1 x = sin x - log x ^ 2

benchRefFunc1 = bench "refFunc1" $ whnf refFunc1 3.141

rdrFunc1 :: Reader Double Double
rdrFunc1 = sin x - log x ^ 2
  where
    x = Reader id

benchRdrFunc1 = bench "rdrFunc1" $ whnf (runReader rdrFunc1) 3.141

-- Result: Identical
