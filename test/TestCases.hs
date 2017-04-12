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
    { nme = "def negate inline"
    , src = "negate(Bln b) -> Bln => false if b else true"
    , tks = [T.Name "negate", T.LParen, T.TypeBln, T.Name "b", T.RParen, T.ThinArrow, T.TypeBln, T.FatArrow, T.False, T.If, T.Name "b", T.Else, T.True]
    , ast = [UFunc $ Func "negate" $ Lambda (Sig [TypedName (TBln Immutable) "b"] (TBln Immutable)) [SExpr $ EIf (ELitBln False) (EName "b") (ELitBln True)]]
    }

  , TestCase
    { nme = "def negate block"

    , src = "negate(Bln b) -> Bln =>\n\
            \    false if b else true"

    , tks = [ T.Name "negate", T.LParen, T.TypeBln, T.Name "b", T.RParen, T.ThinArrow, T.TypeBln, T.FatArrow
            , T.Indent
            , T.False, T.If, T.Name "b", T.Else, T.True
            , T.Dedent]

    , ast = [UFunc $ Func "negate" $ Lambda (Sig [TypedName (TBln Immutable) "b"] $ TBln Immutable) [SExpr $ EIf (ELitBln False) (EName "b") (ELitBln True)]]
    }

  , TestCase
    { nme = "def factorial"

    , src = "factorial(Nat n) -> Nat =>\n\
            \    1 if n <= 0 else n * factorial(n-1)"

    , tks = [ T.Name "factorial", T.LParen, T.TypeNat, T.Name "n", T.RParen, T.ThinArrow, T.TypeNat, T.FatArrow
            , T.Indent
            , T.LitInt 1, T.If, T.Name "n", T.LesserEq, T.LitInt 0, T.Else, T.Name "n", T.Star, T.Name "factorial", T.LParen, T.Name "n", T.Minus, T.LitInt 1, T.RParen
            , T.Dedent]

    , ast = [ UFunc $ Func "factorial" $ Lambda (Sig [TypedName (TNat Immutable) "n"] $ TNat Immutable)
              [ SExpr $ EIf
                (ELitInt 1)
                (EApply $ Apply (ESelect $ Select (EName "n") "<=") [ELitInt 0])
                $ EApply $ Apply (ESelect $ Select (EName "n") "*")
                [EApply $ Apply (EName "factorial") [EApply $ Apply (ESelect $ Select (EName "n") "-") [ELitInt 1]]] ]]
    }

  , TestCase
    { nme = "draw widget"
    , src = "drawWidget(~@, Nat width, Nat height) -> None =>\n\
            \    $ w = Widget(width, height)\n\
            \    if w.exists\n\
            \        w.draw(~@)\n"
    , tks = [ T.Name "drawWidget"
            , T.LParen, T.Tilde, T.At
            , T.Comma, T.TypeNat, T.Name "width"
            , T.Comma, T.TypeNat, T.Name "height"
            , T.RParen, T.ThinArrow, T.TypeNone, T.FatArrow
            , T.Indent
              , T.Dollar, T.Name "w", T.Equal, T.Typename "Widget", T.LParen, T.Name "width", T.Comma, T.Name "height", T.RParen
              , T.Eol
              , T.If, T.Name "w", T.Dot, T.Name "exists"
              , T.Indent
                , T.Name "w", T.Dot, T.Name "draw", T.LParen, T.Tilde, T.At, T.RParen
              , T.Dedent
            , T.Dedent]
    , ast = []
    }
  ]

