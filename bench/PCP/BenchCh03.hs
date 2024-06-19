module PCP.BenchCh03 where

import Criterion.Main
import PCP.Ch03

-- Full bench group ------------------------------------------------------------

benchPCP03 =
  bgroup
    "PCP/Ch03"
    [ benchFibNoMem,
      benchFibMem,
      benchIntermediateList,
      benchAccumulatedList
    ]

-- Individual tests ------------------------------------------------------------

benchFibNoMem = bench "Non-memoized parallel fib" $ whnf (PCP.Ch03.ppNoMem 34) 35

benchFibMem = bench "Memoized parallel fib" $ whnf (PCP.Ch03.ppMem 34) 35

benchIntermediateList = bench "Intermediate list construction" $ whnf sumOddsEvens [0 .. 1000000]

benchAccumulatedList = bench "Accumulating values" $ whnf sumOddsEvens' [0 .. 1000000]
