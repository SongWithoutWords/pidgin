
import Test.Tasty

import TestLexer


main :: IO ()
main = defaultMain tests 

tests :: TestTree
tests = testGroup "Tests" [lexerTests]




-- unitTests :: TestTree
-- unitTests = testGroup "Unit tests"
--   [
--     testCase "Addition" $ 1+1 @?= 2,
--     testCase "Subtraction" $ 1-1 @=? 0
--   ]


