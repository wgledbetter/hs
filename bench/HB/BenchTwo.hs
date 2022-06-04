module HB.BenchTwo where

import Criterion.Main
import HB.Two

-- Full bench group ------------------------------------------------------------

benchTwo = bgroup "HB/Two" [benchHalf]

-- Individual tests ------------------------------------------------------------

benchHalf = bench "Half of three" $ whnf HB.Two.half 3
