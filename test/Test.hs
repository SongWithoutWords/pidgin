
import Test.Tasty
import Test.Tasty.HUnit

import Lexer
import Parser

import TestInput


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests" [lexerTests, parserTests]


lexerTests :: TestTree
lexerTests = testGroup  "lexer" $ map lexTest testCases

lexTest :: TestCase -> TestTree
lexTest tc = testCase (nme tc) $ scanTokens (src tc) @?= tks tc


parserTests :: TestTree
parserTests = testGroup  "parser" $ map parserTest testCases

parserTest :: TestCase -> TestTree
parserTest tc = testCase (nme tc) $ parse (tks tc) @?= ast tc



