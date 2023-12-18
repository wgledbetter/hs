module QuickCheck.HB.TestCh02 where

import HB.Ch02
import Test.Tasty.QuickCheck as QC

propTwoSixTwo x y = HB.Ch02.Ch02SixTwo x y
  where
    types = (x :: Int, y :: Int)

testTwoSixTwo = QC.testProperty "Check answer to 2.6.2" propTwoSixTwo
