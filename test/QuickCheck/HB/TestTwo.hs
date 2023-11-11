module QuickCheck.HB.TestCh2 where

import HB.Ch2
import Test.Tasty.QuickCheck as QC

propTwoSixTwo x y = HB.Ch2.Ch2SixTwo x y
  where
    types = (x :: Int, y :: Int)

testTwoSixTwo = QC.testProperty "Check answer to 2.6.2" propTwoSixTwo
