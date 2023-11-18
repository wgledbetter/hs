module HB.BenchCh02 where

import Criterion.Main
import HB.Ch02

-- Full bench group ------------------------------------------------------------

benchTwo = bgroup "HB/Ch02" [benchHalf]

-- Individual tests ------------------------------------------------------------

benchHalf = bench "Half of three" $ whnf HB.Ch02.half 3
