
import Preface

import Test.Tasty
import Test.Tasty.HUnit

import Lexer
import Parser

import Tokens
import Ast

import TypeCheck -- as Type
-- import qualified TestCases as Test
import TestCases

import TestCase()

import Data.Maybe

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = localOption (mkTimeout 125000) $ testGroup "tests"
  [ lexerTests
  , parserTests
  , typeCheckTests
  ]

lexerTests :: TestTree
lexerTests = testGroup "lexer" $ mapMaybe lexTest testCases

lexTest :: TestCase -> Maybe TestTree
lexTest t = do
  expected <- testTokens t
  actual <- scanTokens <$> testSource t
  return $ testEq (displayName t) actual expected

parserTests :: TestTree
parserTests = testGroup "parser" $ mapMaybe parserTest testCases

parserTest :: TestCase -> Maybe TestTree
parserTest t = tryTestEq
  (displayName t)
  (testAst t)
  (parse <$> tokenInput t)

typeCheckTests :: TestTree
typeCheckTests = testGroup "type checker" $ mapMaybe typeCheckTest testCases

typeCheckTest :: TestCase -> Maybe TestTree
typeCheckTest t = tryTestEq
  (displayName t)
  (testTypeErrors t)
  (snd . typeCheckAst <$> astInput t)

-- TODO: replace tryTestEq :: String -> Maybe a -> Maybe a -> Maybe TestTree
--          with tryTestEq :: TestCase -> (TestCase -> Maybe a) -> (TestCase -> Maybe a) -> Maybe TestTree
-- or better yet tryTestEq :: (TestCase -> Maybe a) -> (TestCase -> Maybe a) -> TestCase -> Maybe TestTree (point-free)

displayName :: TestCase -> String
displayName t = fromMaybe "" $ testName t `orElse` testSource t

tokenInput :: TestCase -> Maybe Tokens
tokenInput t = testTokens t `orElse` fmap scanTokens (testSource t)

astInput :: TestCase -> Maybe Ast
astInput t = testAst t `orElse` fmap parse (tokenInput t)

testEq :: (Eq a, Show a) => String -> a -> a -> TestTree
testEq name actual expected = testCase name $ actual @=? expected

tryTestEq :: (Eq a, Show a) => String -> Maybe a -> Maybe a -> Maybe TestTree
tryTestEq name optActual optExpected = do
  expected <- optExpected
  actual <- optActual
  return $ testEq name actual expected

