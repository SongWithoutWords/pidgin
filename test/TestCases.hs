module TestCases(TestCase(..), testCases) where

import Ast
import qualified Tokens as T


data TestCase
  = TestCase {nme::String, src::String, tks::T.Tokens, ast::Ast}
  deriving(Eq, Show)

type TestCases = [TestCase]


testCases :: TestCases
testCases =
  [ emptyStr
  , defPi
  , ifExpr
  , defNegateInline
  , defNegateBlock
  , defFactorial
  ]

emptyStr :: TestCase
emptyStr = TestCase
  { nme = "empty str"
  , src = ""
  , tks = []
  , ast = []
  }

defPi :: TestCase
defPi = TestCase
  { nme = "def pi"
  , src = "$ pi = 3.14159265"
  , tks = [T.Dollar, T.Name "pi", T.Equal, T.LitFlt 3.14159265]
  , ast = [UnitVariable $ Variable (TypedName (TypeInferred Immutable) "pi") $ ExprLit $ LitFlt 3.14159265]
  }

ifExpr :: TestCase
ifExpr = TestCase
  { nme = "if expr"
  , src = "$ msg = \"it works!\" if true else \"or not :(\""
  , tks = [T.Dollar, T.Name "msg", T.Equal, T.LitStr "it works!", T.If, T.True, T.Else, T.LitStr "or not :("]
  , ast = [UnitVariable $ Variable (TypedName (TypeInferred Immutable) "msg") $ ExprIf $ IfExpr (ExprLit $ LitStr "it works!") (ExprLit $ LitBln True) (ExprLit $ LitStr "or not :(")]
  }

opExpr :: TestCase
opExpr = TestCase
  { nme = "operator expr"
  , src = "$ three = 1 + 2"
  , tks = [T.Dollar, T.Name "msg", T.Equal, T.LitStr "it works!", T.If, T.LitInt 1, T.Lesser, T.LitInt 2, T.Else, T.LitStr "or not :("]
  , ast = [UnitVariable $ Variable (TypedName (TypeInferred Immutable) "pi") $ ExprLit $ LitFlt 3.14159265]
  }

defNegateInline :: TestCase
defNegateInline = TestCase
  { nme = "def negate (one line)"
  , src = "negate(Bln b) -> Bln => false if b else true"
  , tks = [T.Name "negate", T.LParen, T.TypeBln, T.Name "b", T.RParen, T.ThinArrow, T.TypeBln, T.FatArrow, T.False, T.If, T.Name "b", T.Else, T.True]
  , ast = [UnitFunction $ Function (Signature "negate" $ AnonSig Pure [TypedName (TypePrim Immutable PrimBln)"b"] (TypePrim Mutable PrimBln)) []]
  }

defNegateBlock :: TestCase
defNegateBlock = TestCase
  { nme = "def negate (two lines)"

  , src = "negate(Bln b) -> Bln =>\n\
          \    false if b else true"

  , tks = [ T.Name "negate", T.LParen, T.TypeBln, T.Name "b", T.RParen, T.ThinArrow, T.TypeBln, T.FatArrow,
            T.Indent,
            T.False, T.If, T.Name "b", T.Else, T.True,
            T.Dedent]

  , ast = []
  }

defFactorial :: TestCase
defFactorial = TestCase
  { nme = "def factorial"

  , src = "factorial(Int n) -> Int =>\n\
          \    1 if n <= 0 else n * factorial(n-1)"

  , tks = [ T.Name "factorial", T.LParen, T.TypeInt, T.Name "n", T.RParen, T.ThinArrow, T.TypeInt, T.FatArrow
          , T.Indent
          , T.LitInt 1, T.If, T.Name "n", T.LesserOrEq, T.LitInt 0, T.Else, T.Name "n", T.Star, T.Name "factorial", T.LParen, T.Name "n", T.Minus, T.LitInt 1, T.RParen
          , T.Dedent]

  , ast = []
  }


-- class A
--     pub f() -> None
--         a()
--         b = c
--         if q
--             f()
--             g = h
--         x()
--         y = z


