{-# language QuasiQuotes #-}
module Test.Lexer(tests) where

import Control.Monad(unless)
import Data.String.QQ

import Test.Tasty
import qualified Test.Tasty.HUnit as H

import Lexer
import Lexer.FormatTokens
import Lexer.Token as T

test :: String -> String -> Tokens -> TestTree
test name input expected = H.testCase name $
  unless (scanTokens input == expected) $ H.assertFailure msg
    where
      msg = "expected:\n" ++ (formatTokens expected)
        ++ "\nbut got:\n" ++ (formatTokens $ scanTokens input)

tests :: TestTree
tests = testGroup "lexer"
  [ test "empty string" "" []

  , test "indentation a" [s|
.
.
    .
|]
    [ Dot
    , Eol
    , Dot
    , Indent
        , Dot
    , Dedent
    ]

  , test "indentation b" [s|
.
.
        .
|]
    [ Dot
    , Eol
    , Dot, Dot
    ]

  , test "indentation c" [s|
.
.
        .
    .
|]
    [ Dot
    , Eol
    , Dot, Dot
    , Indent
      , Dot
    , Dedent
    ]

  , test "indentation d" [s|
.
    .
.
        .
    .
|]
    [ Dot
    , Indent
      , Dot
    , Dedent
    , Dot, Dot
    , Indent
      , Dot
    , Dedent
    ]

  , test "indentation e" [s|
.
    .
.
    .
|]
    [ Dot
    , Indent
      , Dot
    , Dedent
    , Dot
    , Indent
      , Dot
    , Dedent
    ]

  , test "def pi" "$ pi = 3.14159265" [Dollar, Name "pi", Equal, LitFlt 3.14159265]

  , test "def pi, def e"
    "$ pi = 3.14159265\n$ e = 2.718281828"
    [Dollar, Name "pi", Equal, LitFlt 3.14159265, Eol, Dollar, Name "e", Equal, LitFlt 2.718281828]

  , test "op expr"
    "$ three = 1 + 2"
    [Dollar, Name "three", Equal, LitInt 1, Plus, LitInt 2 ]

  , test "if expr"
    [s|$ msg = "it works!" if true else "or not :("|]
    [ Dollar, Name "msg", Equal
    , LitStr "it works!", If, T.True, Else
    , LitStr "or not :("]

  , test "negate inline"
    "negate(Bln b) -> Bln => false if b else true"
    [ Name "negate", LParen, TypeBln, Name "b", RParen, ThinArrow, TypeBln, FatArrow
    , T.False, If, Name "b", Else, T.True]

  , test "negate block" [s|
negate(Bln b) -> Bln =>
    false if b else true
|]
    [ Name "negate", LParen, TypeBln, Name "b", RParen, ThinArrow, TypeBln, FatArrow
    , Indent
    , T.False, If, Name "b", Else, T.True
    , Dedent
    ]

  , test "factorial" [s|
factorial(Int n) -> Int =>
    1 if n <= 0 else n * factorial(n-1)
|]
    [ Name "factorial", LParen, TypeInt, Name "n", RParen, ThinArrow, TypeInt, FatArrow
    , Indent
    , LitInt 1, If, Name "n", LesserEq, LitInt 0, Else
    , Name "n", Star, Name "factorial", LParen, Name "n", Minus, LitInt 1, RParen
    , Dedent
    ]

  , test "draw widget" [s|
drawWidget(~@, Nat width, Nat height):
    $ w = Widget(width, height)
    if w.exists:
        w.draw(~@)
|]
    [ Name "drawWidget"
    , LParen, Tilde, At
    , Comma, TypeNat, Name "width"
    , Comma, TypeNat, Name "height"
    , RParen, Colon
    , Indent
      , Dollar, Name "w", Equal
        , Typename "Widget", LParen, Name "width", Comma, Name "height", RParen
      , Eol
      , If, Name "w", Dot, Name "exists", Colon
      , Indent
        , Name "w", Dot, Name "draw", LParen, Tilde, At, RParen
      , Dedent
    , Dedent
    ]

  , test "quadratic (explicit return types)" [s|
quadratic(Flt a, Flt b, Flt c) -> Flt -> Flt =>
    (Flt x) -> Flt =>
        a*x*x + b*x + c
|]
    [ Name "quadratic"
      , LParen, TypeFlt, Name "a"
      , Comma, TypeFlt, Name "b"
      , Comma, TypeFlt, Name "c"
      , RParen, ThinArrow , TypeFlt, ThinArrow, TypeFlt, FatArrow
      , Indent
        , LParen, TypeFlt, Name "x", RParen, ThinArrow, TypeFlt, FatArrow
        , Indent
          , Name "a", Star, Name "x", Star, Name "x"
          , Plus
          , Name "b", Star, Name "x"
          , Plus
          , Name "c"
        , Dedent
      , Dedent
      ]
  ]

