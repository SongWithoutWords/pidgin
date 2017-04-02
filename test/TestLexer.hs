module TestLexer(lexerTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Lexer
import Tokens


lexerTests :: TestTree
lexerTests = testGroup "Lexer tests"
  [ emptyStringTest
  , ifTest
  , factorial
  ]

emptyStringTest :: TestTree
emptyStringTest = testCase "Empty string" $ scanTokens "" @=? []

-- TODO: Literals, maybe?


ifSrc :: String
ifSrc =
  "if true\n\
  \    false"

ifLex :: [Token]
ifLex =
  [ TknIf, TknTrue, TknIndent
  , TknFalse, TknDedent]

ifTest :: TestTree
ifTest = testCase "If" $ scanTokens ifSrc @?= ifLex


factorialSrc :: String
factorialSrc =
  "factorial(Int n) -> Int =>\n\
  \    1 if n <= 0 else n * factorial(n-1)"

factorialLex :: [Token]
factorialLex =
  [ TknName "factorial", TknLParen, TknTypeInt, TknName "n", TknRParen, TknThinArrow, TknTypeInt, TknFatArrow
  , TknIndent
  , TknLitInt 1, TknIf, TknName "n", TknLessOrEqual, TknLitInt 0, TknElse, TknName "n", TknStar, TknName "factorial", TknLParen, TknName"n", TknMinus, TknLitInt 1, TknRParen
  , TknDedent]

factorial :: TestTree
factorial = testCase "Indentation" $ scanTokens factorialSrc @?= factorialLex

