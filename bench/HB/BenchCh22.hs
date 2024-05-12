-- Benchmarks for Num, Fractional, and Floating instances of my Reader type

module HB.BenchCh22 where

import Criterion.Main
import HB.Ch22 (Reader (Reader), runReader)

-- Full Bench Group ------------------------------------------------------------

benchCh22 =
  bgroup
    "HB/Ch22"
    [ benchRefFunc1,
      benchRdrFunc1,
      benchRefFunc2,
      benchRdrFunc2,
      benchRefFunc3,
      benchRdrFunc3
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

-- Result: Identical

benchRdrFunc1 = bench "rdrFunc1" $ whnf (runReader rdrFunc1) 3.141

-- Scenario 2: Explicit vs. Reader with recursion and conditional

refFunc2 :: Double -> Double
refFunc2 x
  | x == 1 = 1
  | otherwise = x * refFunc2 (x - 1)

benchRefFunc2 = bench "refFunc2" $ whnf refFunc2 10

-- This is complicated because the if statement can't accept a (Reader x Bool), it has to just be a Bool.
rdrFunc2 :: Reader Double Double -> Reader Double Double
rdrFunc2 x = conditionReader >>= condition
  where
    conditionReader = Reader (\s -> runReader x s == 1)
    condition b = if b then pure 1 else x * rdrFunc2 (x - 1) -- the "if" statement has to exist at the value level, not the reader level.

rdrFunc2' :: Double -> Double
rdrFunc2' = runReader (rdrFunc2 $ Reader id)

benchRdrFunc2 = bench "rdrFunc2" $ whnf rdrFunc2' 10

-- Scenario 3

newton :: (Fractional a) => Int -> (a -> a) -> (a -> a) -> a -> a
newton iters f df x
  | iters == 0 = x
  | otherwise = newton (iters - 1) f df $ x - f x / df x

f3 x = x ^ 4 - 9 * x ^ 3 - 2 * x ^ 2 + 120 * x - 130

df3 x = 4 * x ^ 3 - 27 * x ^ 2 - 4 * x + 120

newtonN = newton 10 f3 df3

refFunc3 :: Double -> Double
refFunc3 = newtonN

rdrFunc3 :: Reader Double Double
rdrFunc3 = newtonN <$> Reader id

benchRefFunc3 = bench "refFunc3" $ whnf refFunc3 5.5

benchRdrFunc3 = bench "rdrFunc3" $ whnf (runReader rdrFunc3) 5.5

-- My conclusion from above: as long as you aren't using bind, reader-defined code sections are identical to their explicit, value-centric counterparts.
