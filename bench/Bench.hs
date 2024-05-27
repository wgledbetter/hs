import BenchMemo
import Criterion.Main
import HB.BenchCh02
import HB.BenchCh22
import Math.BenchDynamical
import Scratch.BenchBTH
import Scratch.BenchTTH

main = defaultMain benches

benches =
  [ -- Memoization
    benchWikiSlowFib33,
    benchWikiMemFib33,
    benchWikiFixFib33,
    -- Haskell Book
    benchTwo,
    benchCh22,
    -- Template Haskell
    benchTTH,
    benchBTH,
    -- Math Stuff
    benchDynamical
  ]
