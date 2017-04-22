
import Test.Tasty
import Test.Tasty.HUnit

import Lexer
import Parser
import qualified TypeCheck as Type

import qualified TestCases as Test

import Data.Maybe


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests" [lexerTests, parserTests, typeCheckTests]


lexerTests :: TestTree
lexerTests = testGroup "lexer" $ mapMaybe lexTest Test.testCases

lexTest :: Test.TestCase -> Maybe TestTree
lexTest t = do
  expected <- Test.tokens t
  let actual = scanTokens $ Test.source t
  return $ testCase (Test.name t) $ actual @?= expected

parserTests :: TestTree
parserTests = testGroup "parser" $ mapMaybe parserTest Test.testCases

parserTest :: Test.TestCase -> Maybe TestTree
parserTest t = do
  expected <- Test.ast t
  let tokens = fromMaybe (scanTokens $ Test.source t) (Test.tokens t)
  let actual = parse tokens
  return $ testCase (Test.name t) $ actual @?= expected

typeCheckTests :: TestTree
typeCheckTests = testGroup "type checker" $ mapMaybe typeCheckTest Test.testCases

typeCheckTest :: Test.TestCase -> Maybe TestTree
typeCheckTest t = do
  expected <- Test.typeErrors t
  let tkns = fromMaybe (scanTokens $ Test.source t) (Test.tokens t)
  let ast = fromMaybe (parse tkns) (Test.ast t)
  let actual = Type.errors $ Type.typeCheck ast
  return $ testCase (Test.name t) $ actual @?= expected


