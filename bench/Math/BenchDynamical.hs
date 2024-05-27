module Math.BenchDynamical where

import Criterion.Main
import Math.Dynamical

-- Full Bench Group ------------------------------------------------------------

benchDynamical =
  bgroup
    "Math/Dynamical"
    [ benchEulerStep1,
      benchMidpointStep1,
      benchRKF45Step1
    ]

-- Individual Tests ------------------------------------------------------------

-- 1D Autonomous
dyn1 = OneD (-1)

step1 :: ButcherTableau -> [Double]
step1 bt = rkStep bt dyn1 1 [-1, 1]

-- Baseline = 66.19 ns
benchEulerStep1 = bench "eulerStep1" $ whnf step1 euler

-- Baseline = 90.87 ns
benchMidpointStep1 = bench "midpointStep1" $ whnf step1 midpoint

-- Baseline = 173.1 ns
benchRKF45Step1 = bench "rkf45Step1" $ whnf step1 rkf45
