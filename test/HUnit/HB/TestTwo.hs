module HUnit.HB.TestTwo where

import HB.Two
  ( half,
    square,
    twoFiveThree,
    twoFiveTwo,
    twoSixOne,
    twoSixOneAnswer,
  )
import qualified Test.Tasty as Test.Tasty.Core
import Test.Tasty.HUnit
  ( assertBool,
    assertEqual,
    testCase,
  )

-- 2.5 -------------------------------------------------------------------------

testHalf :: Test.Tasty.Core.TestTree
testHalf =
  testCase
    "test half function"
    $ assertEqual
      "Half of five is two and a half"
      (HB.Two.half 5.0)
      2.5

testSquare :: Test.Tasty.Core.TestTree
testSquare =
  testCase
    "test square function"
    $ assertEqual
      "Five squared is twenty-five"
      (HB.Two.square 5)
      25

testTwoFiveTwo :: Test.Tasty.Core.TestTree
testTwoFiveTwo =
  testCase
    "test answer to 2.5.2"
    $ assertEqual
      "Area of circle with radius 4 is roughly 50.24"
      (HB.Two.twoFiveTwo 4)
      50.24

testTwoFiveThree :: Test.Tasty.Core.TestTree
testTwoFiveThree =
  testCase
    "test answer to 2.5.3"
    $ assertEqual
      "Area of circle with radius 3 is much closer to 28.269"
      (HB.Two.twoFiveThree 3)
      $ pi * 3 * 3

-- 2.6 -------------------------------------------------------------------------

testTwoSixOne :: Test.Tasty.Core.TestTree
testTwoSixOne =
  testCase
    "test solution to 2.6.1"
    $ assertBool
      "parenthesis work like this"
      HB.Two.twoSixOneAnswer
