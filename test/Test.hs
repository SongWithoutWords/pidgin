import Preface

import Test.Tasty
import Test.Tasty.HUnit

import Lexer
import Parser

import Ast
import MultiMapAst
import Tokens

import TypeCheck
import TestCases

import TestCase()

import Data.Maybe

main :: IO ()
main = defaultMain tests

testTimeoutMs = 10000

tests :: TestTree
tests = localOption (mkTimeout testTimeoutMs) $ testGroup "tests"
  [ lexerTests
  , parserTests
  , typeInferenceTests
  , typeCheckTests
  ]

lexerTests :: TestTree
lexerTests = testGroup "lexer" $ mapMaybe lexTest testCases

lexTest :: TestCase -> Maybe TestTree
lexTest t = tryTestEq
  (displayName t)
  (testTokens t)
  (scanTokens <$> testSource t)

parserTests :: TestTree
parserTests = testGroup "parser" $ mapMaybe parserTest testCases

parserTest :: TestCase -> Maybe TestTree
parserTest t = tryTestEq
  (displayName t)
  (testAst t)
  (parse <$> tokenInput t)

typeInferenceTests :: TestTree
typeInferenceTests = testGroup "type inference" $ mapMaybe typeInferenceTest testCases

typeInferenceTest :: TestCase -> Maybe TestTree
typeInferenceTest t = tryTestEq
  (displayName t)
  (testTypedAst t)
  (fst . typeCheckAst . multiMapAst <$> astInput t)

typeCheckTests :: TestTree
typeCheckTests = testGroup "type checker" $ mapMaybe typeCheckTest testCases

typeCheckTest :: TestCase -> Maybe TestTree
typeCheckTest t = tryTestEq
  (displayName t)
  (testTypeErrors t)
  (snd . typeCheckAst . multiMapAst <$> astInput t)

-- TODO: replace tryTestEq :: String -> Maybe a -> Maybe a -> Maybe TestTree
--          with tryTestEq :: TestCase -> (TestCase -> Maybe a) -> (TestCase -> Maybe a) -> Maybe TestTree
-- or better yet tryTestEq :: (TestCase -> Maybe a) -> (TestCase -> Maybe a) -> TestCase -> Maybe TestTree (point-free)

displayName :: TestCase -> String
displayName t = fromMaybe "" $ testName t `orElse` testSource t

tokenInput :: TestCase -> Maybe Tokens
tokenInput t = testTokens t `orElse` (scanTokens <$> testSource t)

astInput :: TestCase -> Maybe AstLu
astInput t = testAst t `orElse` (parse <$> tokenInput t)

testEq :: (Eq a, Show a) => String -> a -> a -> TestTree
testEq name actual expected = testCase name $ actual @=? expected

tryTestEq :: (Eq a, Show a) => String -> Maybe a -> Maybe a -> Maybe TestTree
tryTestEq name optActual optExpected = do
  expected <- optExpected
  actual <- optActual
  return $ testEq name actual expected

