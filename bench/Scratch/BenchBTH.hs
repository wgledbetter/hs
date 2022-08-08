module Scratch.BenchBTH where

import Criterion.Main
import Scratch.BTH1
import Scratch.BTH2

-- Full bench group ------------------------------------------------------------

benchBTH = bgroup "Scratch/BTH" [benchPower5, benchPower5', benchTPower5]

-- Individual benches ----------------------------------------------------------

benchPower5 = bench "Five explicit multiplications" $ whnf power5 3

benchPower5' = bench "Recursive specialization to 5" $ whnf power5' 3

benchTPower5 = bench "Template haskell recursion" $ whnf tPower5 3
