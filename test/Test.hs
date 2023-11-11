import qualified HUnit.HB.TestCh2
import qualified QuickCheck.HB.TestCh2
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
    [QuickCheck.HB.TestCh2.testTwoSixTwo]

-- HUnit Unit Tests ------------------------------------------------------------

unitTests :: TestTree
unitTests =
  testGroup
    "HUnit Tests"
    [ HUnit.HB.TestCh2.testHalf,
      HUnit.HB.TestCh2.testSquare,
      HUnit.HB.TestCh2.testTwoFiveTwo,
      HUnit.HB.TestCh2.testTwoFiveThree,
      HUnit.HB.TestCh2.testTwoSixOne,
      HUnit.HB.TestCh2.testParenOne,
      HUnit.HB.TestCh2.testParenTwo,
      HUnit.HB.TestCh2.testParenThree
    ]
