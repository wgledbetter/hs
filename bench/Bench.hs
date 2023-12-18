import BenchMemo
import Criterion.Main
import HB.BenchCh02
import Scratch.BenchBTH
import Scratch.BenchTTH

main = defaultMain benches

benches =
  [
    -- Memoization
    benchWikiSlowFib33,
    benchWikiMemFib33,
    benchWikiFixFib33,
    -- Haskell Book
    benchTwo,
    -- Template Haskell
    benchTTH,
    benchBTH
  ]
