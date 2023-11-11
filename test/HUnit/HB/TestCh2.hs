module HUnit.HB.TestCh2 where

import HB.Ch2
  ( half,
    parenOne,
    parenThree,
    parenTwo,
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
      (HB.Ch2.half 5.0)
      2.5

testSquare :: Test.Tasty.Core.TestTree
testSquare =
  testCase
    "test square function"
    $ assertEqual
      "Five squared is twenty-five"
      (HB.Ch2.square 5)
      25

testTwoFiveTwo :: Test.Tasty.Core.TestTree
testTwoFiveTwo =
  testCase
    "test answer to 2.5.2"
    $ assertEqual
      "Area of circle with radius 4 is roughly 50.24"
      (HB.Ch2.Ch2FiveTwo 4)
      50.24

testTwoFiveThree :: Test.Tasty.Core.TestTree
testTwoFiveThree =
  testCase
    "test answer to 2.5.3"
    $ assertEqual
      "Area of circle with radius 3 is much closer to 28.269"
      (HB.Ch2.Ch2FiveThree 3)
      $ pi * 3 * 3

-- 2.6 -------------------------------------------------------------------------

testTwoSixOne :: Test.Tasty.Core.TestTree
testTwoSixOne =
  testCase
    "test solution to 2.6.1"
    $ assertBool
      "parentheses work like this"
      HB.Ch2.Ch2SixOneAnswer

-- 2.11 ------------------------------------------------------------------------

testParenOne :: Test.Tasty.Core.TestTree
testParenOne =
  testCase
    "test solution to 2.11.1"
    $ assertBool
      "Parentheses work like this"
      HB.Ch2.parenOne

testParenTwo :: Test.Tasty.Core.TestTree
testParenTwo =
  testCase
    "test solution to 2.11.2"
    $ assertBool
      "Parentheses work like this"
      HB.Ch2.parenTwo

testParenThree :: Test.Tasty.Core.TestTree
testParenThree =
  testCase
    "test solution to 2.11.3"
    $ assertBool
      "Parentheses work like this"
      HB.Ch2.parenThree
