import Criterion.Main
import HB.BenchCh02
import Scratch.BenchBTH
import Scratch.BenchTTH

main = defaultMain benches

benches =
  [ benchTwo,
    benchTTH,
    benchBTH
  ]
