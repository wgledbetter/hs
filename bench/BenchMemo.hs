module BenchMemo where

import Criterion.Main
import Memo

-- The Wiki Page ---------------------------------------------------------------

benchWikiSlowFib33 = bench "Naive fibonacci" $ whnf slowFib 33

benchWikiMemFib33 = bench "Simple fibonacci memoization" $ whnf memoizedFib 33

benchWikiFixFib33 = bench "Fixed-point fibonacci memoization" $ whnf fixedFib 33
