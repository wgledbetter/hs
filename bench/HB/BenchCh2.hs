module HB.BenchCh2 where

import Criterion.Main
import HB.Ch2

-- Full bench group ------------------------------------------------------------

benchTwo = bgroup "HB/Ch2" [benchHalf]

-- Individual tests ------------------------------------------------------------

benchHalf = bench "Half of three" $ whnf HB.Ch2.half 3
