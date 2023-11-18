import qualified HUnit.HB.TestCh02
import qualified QuickCheck.HB.TestCh02
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Test.Tasty.SmallCheck as SC

-- Main ------------------------------------------------------------------------

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [qcProps, unitTests]

-- QuickCheck Properties -------------------------------------------------------

qcProps :: TestTree
qcProps =
  testGroup
    "QuickCheck Tests"
    [QuickCheck.HB.TestCh02.testTwoSixTwo]

-- HUnit Unit Tests ------------------------------------------------------------

unitTests :: TestTree
unitTests =
  testGroup
    "HUnit Tests"
    [ HUnit.HB.TestCh02.testHalf,
      HUnit.HB.TestCh02.testSquare,
      HUnit.HB.TestCh02.testTwoFiveTwo,
      HUnit.HB.TestCh02.testTwoFiveThree,
      HUnit.HB.TestCh02.testTwoSixOne,
      HUnit.HB.TestCh02.testParenOne,
      HUnit.HB.TestCh02.testParenTwo,
      HUnit.HB.TestCh02.testParenThree
    ]
