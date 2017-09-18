module Tests where

import Control.Applicative((<|>))
import Data.Maybe

import Test.Tasty
import Test.Tasty.HUnit

import Preface

import Ast
import Source
import Transforms
import Tokens

-- import TypeCheck
import TestCases

import TestCase()

testTimeout_μs = 10000

main :: IO ()
main = do
  tests' <- tests
  defaultMain tests'

tests :: IO TestTree
tests = do
  returnValTests' <- returnValTests
  return $ localOption (mkTimeout testTimeout_μs) $ testGroup "tests"
    [ lexerTests
    , parserTests
    , typedAstTests
    , typeErrorTests
    , returnValTests'
    ]


lexerTests :: TestTree
lexerTests = testGroup "lexer" $ mapMaybe lexTest testCases

lexTest :: TestCase -> Maybe TestTree
lexTest = tryTestEq testTokens (\t -> scanTokens <$> testSource t)

parserTests :: TestTree
parserTests = testGroup "parser" $ mapMaybe parserTest testCases

parserTest :: TestCase -> Maybe TestTree
parserTest = tryTestEq testAst (\t -> parse <$> tokenInput t)

typedAstTests :: TestTree
typedAstTests = testGroup "typed ast" $ mapMaybe typedAstTest testCases

typedAstTest :: TestCase -> Maybe TestTree
typedAstTest = tryTestEq testTypedAst (\t -> typedAstFromParseTree <$> astInput t)

typeErrorTests :: TestTree
typeErrorTests = testGroup "type errors" $ mapMaybe typeErrorTest testCases

typeErrorTest :: TestCase -> Maybe TestTree
typeErrorTest = tryTestEq testTypeErrors (\t -> typeErrorsFromParseTree <$> astInput t)

returnValTests :: IO TestTree
returnValTests = do
  tests <- sequence $ traverse returnValTest testCases
  return $ testGroup "type errors" (mapMaybe tests)

returnValTest :: TestCase -> IO (Maybe TestTree)
returnValTest t = do
  maybeActual <- returnValFromSource <$> testSource t
  return $ tryTestEq' maybeActual (testReturnVal t)


tokenInput :: TestCase -> Maybe Tokens
tokenInput t = testTokens t <|> (scanTokens <$> testSource t)

astInput :: TestCase -> Maybe Ast0
astInput t = testAst t <|> (parse <$> tokenInput t)


tryTestEq :: (Eq a, Show a)
  => (TestCase -> Maybe a)
  -> (TestCase -> Maybe a)
  -> TestCase
  -> Maybe TestTree
tryTestEq getActual getExpected testCase =
  tryTestEq' (getActual testCase) (getExpected testCase) testCase

tryTestEq' :: (Eq a, Show a) => Maybe a -> Maybe a -> TestCase -> Maybe TestTree
tryTestEq' actual expected testCase = do
  actual' <- actual
  expected' <- expected
  let name = displayName testCase
  return $ testEq name actual' expected'

testEq :: (Eq a, Show a) => String -> a -> a -> TestTree
testEq name actual expected = testCase name $ actual @=? expected

displayName :: TestCase -> String
displayName t = testName t ?? (case testSource t of
  (Just (SourceCode s)) -> s
  _ -> "")

