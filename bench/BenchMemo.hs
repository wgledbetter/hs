module BenchMemo where

import Criterion.Main
import Memo

-- The Wiki Page ---------------------------------------------------------------

benchWikiSlowFib33 = bench "Naive fibonacci" $ whnf slow_fib 33

benchWikiMemFib33 = bench "Simple fibonacci memoization" $ whnf memoized_fib 33

benchWikiFixFib33 = bench "Fixed-point fibonacci memoization" $ whnf fixed_fib 33
