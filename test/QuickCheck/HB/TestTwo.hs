module QuickCheck.HB.TestTwo where

import HB.Two
import Test.Tasty.QuickCheck as QC

propTwoSixTwo x y = HB.Two.twoSixTwo x y
  where
    types = (x :: Int, y :: Int)

testTwoSixTwo = QC.testProperty "Check answer to 2.6.2" propTwoSixTwo
