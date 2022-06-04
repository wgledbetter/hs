import qualified HUnit.HB.TestTwo
import qualified QuickCheck.HB.TestTwo
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
    [QuickCheck.HB.TestTwo.testTwoSixTwo]

-- HUnit Unit Tests ------------------------------------------------------------

unitTests :: TestTree
unitTests =
  testGroup
    "HUnit Tests"
    [ HUnit.HB.TestTwo.testHalf,
      HUnit.HB.TestTwo.testSquare,
      HUnit.HB.TestTwo.testTwoFiveTwo,
      HUnit.HB.TestTwo.testTwoFiveThree,
      HUnit.HB.TestTwo.testTwoSixOne
    ]
