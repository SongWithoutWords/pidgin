-- module Tests where

import Control.Applicative((<|>))
import Data.Maybe

import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as H

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
main = T.defaultMain tests

tests :: T.TestTree
tests = T.localOption (T.mkTimeout testTimeout_μs) $ T.testGroup "tests"
    [ lexerTests
    , parserTests
    , typedAstTests
    , typeErrorTests
    , returnValTests
    ]

lexerTests :: T.TestTree
lexerTests = T.testGroup "lexer" $ mapMaybe lexTest testCases

lexTest :: TestCase -> Maybe T.TestTree
lexTest = tryTestEq testTokens (\t -> scanTokens <$> testSource t)

parserTests :: T.TestTree
parserTests = T.testGroup "parser" $ mapMaybe parserTest testCases

parserTest :: TestCase -> Maybe T.TestTree
parserTest = tryTestEq testAst (\t -> parse <$> tokenInput t)

typedAstTests :: T.TestTree
typedAstTests = T.testGroup "typed ast" $ mapMaybe typedAstTest testCases

typedAstTest :: TestCase -> Maybe T.TestTree
typedAstTest = tryTestEq testTypedAst (\t -> typedAstFromParseTree <$> astInput t)

typeErrorTests :: T.TestTree
typeErrorTests = T.testGroup "type errors" $ mapMaybe typeErrorTest testCases

typeErrorTest :: TestCase -> Maybe T.TestTree
typeErrorTest = tryTestEq testTypeErrors (\t -> typeErrorsFromParseTree <$> astInput t)

returnValTests :: T.TestTree
returnValTests = T.testGroup "return values" $ mapMaybe returnValTest testCases

returnValTest :: TestCase -> Maybe T.TestTree
returnValTest t = do
  expected <- testReturnVal t
  source <- testSource t
  let name = displayName t
  return $ H.testCase name $ do
    result <- returnValFromSource source
    case result of
      Nothing -> putStrLn "No return value"
      Just actual -> actual H.@=? expected

tokenInput :: TestCase -> Maybe Tokens
tokenInput t = testTokens t <|> (scanTokens <$> testSource t)

astInput :: TestCase -> Maybe Ast0
astInput t = testAst t <|> (parse <$> tokenInput t)


tryTestEq :: (Eq a, Show a)
  => (TestCase -> Maybe a)
  -> (TestCase -> Maybe a)
  -> TestCase
  -> Maybe T.TestTree
tryTestEq getActual getExpected testCase =
  tryTestEq' (getActual testCase) (getExpected testCase) testCase

tryTestEq' :: (Eq a, Show a) => Maybe a -> Maybe a -> TestCase -> Maybe T.TestTree
tryTestEq' actual expected testCase = do
  actual' <- actual
  expected' <- expected
  return $ testEq actual' expected' testCase

testEq :: (Eq a, Show a) => a -> a -> TestCase -> T.TestTree
testEq actual expected testCase =
  let name = displayName testCase
  in H.testCase name $ actual H.@=? expected

displayName :: TestCase -> String
displayName t = testName t ?? case testSource t of
  Just (SourceCode s) -> s
  _ -> ""

