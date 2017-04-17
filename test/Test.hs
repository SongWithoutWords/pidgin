
import Test.Tasty
import Test.Tasty.HUnit

import Lexer
import Parser

import TestCases

import Data.Maybe


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests" [lexerTests, parserTests]


lexerTests :: TestTree
lexerTests = testGroup  "lexer" $ mapMaybe lexTest testCases

lexTest :: TestCase -> Maybe TestTree
lexTest test = do
  expected <- tks test
  return $ testCase (nme test) $ scanTokens (src test) @?= expected

parserTests :: TestTree
parserTests = testGroup  "parser" $ mapMaybe parserTest testCases

parserTest :: TestCase -> Maybe TestTree
parserTest test = do
  expected <- ast test
  let tokens = fromMaybe (scanTokens $ src test) (tks test)
  return $ testCase (nme test) $ parse tokens @?= expected



