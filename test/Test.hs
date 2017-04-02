import Test.Tasty
--import Test.Tast.SmallCheck as SC
import Test.Tasty.HUnit



main :: IO ()
main = defaultMain unitTests --putStrLn "Test suite not yet implemented"

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [
    testCase "Addition" $ 1+1 @?= 2,
    testCase "Subtraction" $ 1-1 @=? 0
  ]


