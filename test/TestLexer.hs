module TestLexer(lexerTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Lexer
import Tokens

import qualified TestInput as In


lexerTests :: TestTree
lexerTests = testGroup "Lexer tests"
  [ emptyStringTest
  , notFnTests
  , factorialFnTest
  ]


lexTest :: String -> String -> Tokens -> TestTree
lexTest name input expectedOutput = testCase name $ scanTokens input @?= expectedOutput


emptyStringTest :: TestTree
emptyStringTest = lexTest "empty string" "" []


notFnTests :: TestTree
notFnTests = testGroup "not function"
  [ l1
  , l2
  , l3
  , l6
  ]
  where
    l1 = lexTest "L1" In.defNotL1 $ signature ++ bodyL1
    l2 = lexTest "L2" In.defNotL2 $ signature ++ bodyIndented bodyL1
    l3 = lexTest "L3" In.defNotL3 $ signature ++ bodyIndented bodyL2
    l6 = lexTest "L6" In.defNotL6 $ signature ++ bodyIndented bodyL2

    bodyIndented body = [TknIndent] ++ body ++ [TknDedent]

    signature = [TknName "myNot", TknLParen, TknTypeBln, TknName "b", TknRParen, TknThinArrow, TknTypeBln, TknFatArrow]

    bodyL1 = [TknFalse, TknIf, TknName "b", TknElse, TknTrue]
    bodyL2 = [TknFalse, TknIf, TknName "b", TknElse, TknEol, TknTrue]


factorialFnTest :: TestTree
factorialFnTest = lexTest "factorial function" In.defFactorial tokens
  where
    tokens =
      [ TknName "factorial", TknLParen, TknTypeInt, TknName "n", TknRParen, TknThinArrow, TknTypeInt, TknFatArrow
      , TknIndent
      , TknLitInt 1, TknIf, TknName "n", TknLessOrEqual, TknLitInt 0, TknElse, TknName "n", TknStar, TknName "factorial", TknLParen, TknName"n", TknMinus, TknLitInt 1, TknRParen
      , TknDedent]

