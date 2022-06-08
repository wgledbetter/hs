module Scratch.BenchTTH where

import Criterion.Main
import Scratch.TTH1 (standardLookupTable)
import Scratch.TTH2 (templatedLookupTable)

-- Full bench group ------------------------------------------------------------

benchTTH = bgroup "Scratch/TTH" [benchStandard, benchTemplate]

-- Individual tests ------------------------------------------------------------

benchStandard = bench "Standard lookup table" $ whnf standardLookupTable 2

benchTemplate = bench "Template lookup table" $ whnf templatedLookupTable 2
