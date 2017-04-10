module TestCases(TestCase(..), testCases) where

import Ast
import qualified Tokens as T


data TestCase
  = TestCase {nme::String, src::String, tks::T.Tokens, ast::Ast}
  deriving(Eq, Show)

type TestCases = [TestCase]


testCases :: TestCases
testCases =
  [ TestCase
    { nme = "empty str"
    , src = ""
    , tks = []
    , ast = []
    }

  , TestCase
    { nme = "def pi"
    , src = "$ pi = 3.14159265"
    , tks = [T.Dollar, T.Name "pi", T.Equal, T.LitFlt 3.14159265]
    , ast = [UVar $ Var (TypedName (TInferred Immutable) "pi") $ ELitFlt 3.14159265]
    }

  , TestCase
    { nme = "if expr"
    , src = "$ msg = \"it works!\" if true else \"or not :(\""
    , tks = [T.Dollar, T.Name "msg", T.Equal, T.LitStr "it works!", T.If, T.True, T.Else, T.LitStr "or not :("]
    , ast = [UVar $ Var (TypedName (TInferred Immutable) "msg") $ EIf (ELitStr "it works!") (ELitBln True) (ELitStr "or not :(")]
    }

  , TestCase
    { nme = "op expr"
    , src = "$ three = 1 + 2"
    , tks = [T.Dollar, T.Name "three", T.Equal, T.LitInt 1, T.Plus, T.LitInt 2]
    , ast = [UVar $ Var (TypedName (TInferred Immutable) "three") $ EApply $ Apply (ESelect $ Select (ELitInt 1) "+") [ELitInt 2]]
    }

  , TestCase
    { nme = "op expr"
    , src = "$ three = 1 + 2"
    , tks = [T.Dollar, T.Name "three", T.Equal, T.LitInt 1, T.Plus, T.LitInt 2]
    , ast = [UVar $ Var (TypedName (TInferred Immutable) "three") $ EApply $ Apply (ESelect $ Select (ELitInt 1) "+") [ELitInt 2]]
    }

  , TestCase
    { nme = "def negate inline"
    , src = "negate(Bln b) -> Bln => false if b else true"
    , tks = [T.Name "negate", T.LParen, T.TypeBln, T.Name "b", T.RParen, T.ThinArrow, T.TypeBln, T.FatArrow, T.False, T.If, T.Name "b", T.Else, T.True]
    , ast = [UFunc $ Func (Sig "negate" $ AnonSig Pure [TypedName (TBln Immutable) "b"] (TBln Mutable)) [SExpr $ EIf (ELitBln False) (EName "b") (ELitBln True)]]
    }

  , TestCase
    { nme = "def negate block"

    , src = "negate(Bln b) -> Bln =>\n\
            \    false if b else true"

    , tks = [ T.Name "negate", T.LParen, T.TypeBln, T.Name "b", T.RParen, T.ThinArrow, T.TypeBln, T.FatArrow,
              T.Indent,
              T.False, T.If, T.Name "b", T.Else, T.True,
              T.Dedent]

    , ast = []
    }

  , TestCase
    { nme = "def factorial"

    , src = "factorial(Int n) -> Int =>\n\
            \    1 if n <= 0 else n * factorial(n-1)"

    , tks = [ T.Name "factorial", T.LParen, T.TypeInt, T.Name "n", T.RParen, T.ThinArrow, T.TypeInt, T.FatArrow
            , T.Indent
            , T.LitInt 1, T.If, T.Name "n", T.LesserOrEq, T.LitInt 0, T.Else, T.Name "n", T.Star, T.Name "factorial", T.LParen, T.Name "n", T.Minus, T.LitInt 1, T.RParen
            , T.Dedent]

    , ast = []
    }

  , TestCase
    { nme = "draw widget"
    , src = "drawWidget(Nat width, Nat height) -> None\n\
            \    $ w = Widget(width, height)\n\
            \    if w.exists\n\
            \        w.@draw()\n"
    , tks = []
    , ast = []
    }
  ]

