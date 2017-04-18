module TestCases(TestCase(..), testCases) where

import Ast
import qualified Tokens as T


data TestCase
  = TestCase { nme :: String, src :: String, tks :: Maybe T.Tokens, ast :: Maybe Ast}
  deriving(Eq, Show)

type TestCases = [TestCase]


testCases :: TestCases
testCases =
  [ TestCase
    { nme = "empty str"
    , src = ""
    , tks = Just []
    , ast = Just []
    }

  , TestCase
    { nme = "def pi"
    , src = "$ pi = 3.14159265"
    , tks = Just [ T.Dollar, T.Name "pi", T.Equal, T.LitFlt 3.14159265 ]
    , ast = Just
      [ UVar
        $ Var
          (TypedName (TInferred Immutable) "pi")
          $ ELitFlt 3.14159265 ]
    }

  , TestCase
    { nme = "if expr"
    , src = "$ msg = \"it works!\" if true else \"or not :(\""
    , tks = Just [ T.Dollar, T.Name "msg", T.Equal, T.LitStr "it works!", T.If, T.True, T.Else, T.LitStr "or not :(" ]
    , ast = Just
      [ UVar
        $ Var
          (TypedName (TInferred Immutable) "msg")
          $ EIf (ELitStr "it works!") (ELitBln True) (ELitStr "or not :(") ]
    }

  , TestCase
    { nme = "op expr"
    , src = "$ three = 1 + 2"
    , tks = Just [ T.Dollar, T.Name "three", T.Equal, T.LitInt 1, T.Plus, T.LitInt 2 ]
    , ast = Just
      [ UVar
        $ Var
          (TypedName (TInferred Immutable) "three")
          $ EAdd (ELitInt 1) (ELitInt 2) ]
    }

  , TestCase
    { nme = "def negate inline"
    , src = "negate(Bln b) -> Bln => false if b else true"
    , tks = Just
      [T.Name "negate", T.LParen, T.TypeBln, T.Name "b", T.RParen, T.ThinArrow, T.TypeBln, T.FatArrow
      , T.False, T.If, T.Name "b", T.Else, T.True]

    , ast = Just
      [ UFunc
        $ Func "negate"
          $ Lambda
            (Sig Pure [TypedName (TBln Immutable) "b"] (TBln Immutable))
            [SExpr $ EIf (ELitBln False) (EName "b") (ELitBln True)] ]
    }

  , TestCase
    { nme = "def negate block"
    , src = "negate(Bln b) -> Bln =>\n\
            \    false if b else true"
    , tks = Just
      [ T.Name "negate", T.LParen, T.TypeBln, T.Name "b", T.RParen, T.ThinArrow, T.TypeBln, T.FatArrow
      , T.Indent
      , T.False, T.If, T.Name "b", T.Else, T.True
      , T.Dedent]

    , ast = Just
      [ UFunc
        $ Func "negate"
          $ Lambda
            (Sig Pure [TypedName (TBln Immutable) "b"] $ TBln Immutable)
            [SExpr $ EIf (ELitBln False) (EName "b") (ELitBln True)] ]
    }

  , TestCase
    { nme = "def factorial"

    , src = "factorial(Nat n) -> Nat =>\n\
            \    1 if n <= 0 else n * factorial(n-1)"

    , tks = Just
      [ T.Name "factorial", T.LParen, T.TypeNat, T.Name "n", T.RParen, T.ThinArrow, T.TypeNat, T.FatArrow
      , T.Indent
      , T.LitInt 1, T.If, T.Name "n", T.LesserEq, T.LitInt 0, T.Else
      , T.Name "n", T.Star, T.Name "factorial", T.LParen, T.Name "n", T.Minus, T.LitInt 1, T.RParen
      , T.Dedent]

    , ast = Just
      [ UFunc
        $ Func "factorial"
          $ Lambda
            (Sig Pure [TypedName (TNat Immutable) "n"] $ TNat Immutable)
            [ SExpr
              $ EIf
                (ELitInt 1)
                (ELesserEq (EName "n") (ELitInt 0))
                (EMul
                    (EName "n")
                    $ EApply $ Apply
                      (EName "factorial")
                      Pure
                      [ESub (EName "n") (ELitInt 1)]
                )
            ]
      ]
    }

  , TestCase
    { nme = "clothing (cascading if exprs inline)"
    , src =
      "clothing(Weather w) -> Clothing =>\n\
      \    rainCoat if w.isRaining else coat if w.isCold else tShirt if w.isSunny else jacket"
    , tks = Nothing
    , ast = Just
    [ UFunc $ Func "clothing" $ Lambda
      ( Sig Pure [TypedName (TUser Immutable "Weather") "w"] $ TUser Immutable "Clothing" )
      [ SExpr
        $ EIf (EName "rainCoat") (ESelect $ Select (EName "w") "isRaining")
        $ EIf (EName "coat") (ESelect $ Select (EName "w") "isCold")
        $ EIf (EName "tShirt") (ESelect $ Select (EName "w") "isSunny")
        $ EName "jacket"
      ]
    ]
    }

  , TestCase
    { nme = "draw widget (imperative-style if)"
    , src = "drawWidget(~@, Nat width, Nat height) -> None =>\n\
            \    $ w = Widget(width, height)\n\
            \    if w.exists\n\
            \        w.draw(~@)\n"
    , tks = Just
      [ T.Name "drawWidget"
      , T.LParen, T.Tilde, T.At
      , T.Comma, T.TypeNat, T.Name "width"
      , T.Comma, T.TypeNat, T.Name "height"
      , T.RParen, T.ThinArrow, T.TypeNone, T.FatArrow
      , T.Indent
        , T.Dollar, T.Name "w", T.Equal
          , T.Typename "Widget", T.LParen, T.Name "width", T.Comma, T.Name "height", T.RParen
        , T.Eol
        , T.If, T.Name "w", T.Dot, T.Name "exists"
        , T.Indent
          , T.Name "w", T.Dot, T.Name "draw", T.LParen, T.Tilde, T.At, T.RParen
        , T.Dedent
      , T.Dedent ]

    , ast = Just
      [ UFunc
        $ Func "drawWidget"
          $ Lambda
            ( Sig WriteWorld [TypedName (TNat Immutable) "width", TypedName (TNat Immutable) "height"] TNone )
            [ SVar
              $ Var
                ( TypedName (TInferred Immutable) "w") (ECons $ Cons "Widget" Pure [EName "width", EName "height"])
            , SIf
              $ Iff
                $ CondBlock
                  ( ESelect $ Select (EName "w") "exists" )
                  [ SExpr $ EApply $ Apply (ESelect $ Select (EName "w") "draw") WriteWorld [] ]
            ]
      ]
    }

  , TestCase
    { nme = "quadratic"
    , src = "quadratic(Flt a, Flt b, Flt c) -> Flt -> Flt =>\n\
            \    (Flt x) -> Flt =>\n\
            \        a*x*x + b*x + c"

    , tks = Just
      [ T.Name "quadratic"
        , T.LParen, T.TypeFlt, T.Name "a"
        , T.Comma, T.TypeFlt, T.Name "b"
        , T.Comma, T.TypeFlt, T.Name "c"
        , T.RParen, T.ThinArrow , T.TypeFlt, T.ThinArrow, T.TypeFlt, T.FatArrow
        , T.Indent
          , T.LParen, T.TypeFlt, T.Name "x", T.RParen, T.ThinArrow, T.TypeFlt, T.FatArrow
          , T.Indent
            , T.Name "a", T.Star, T.Name "x", T.Star, T.Name "x"
            , T.Plus
            , T.Name "b", T.Star, T.Name "x"
            , T.Plus
            , T.Name "c"
          , T.Dedent
        , T.Dedent ]

    , ast = Just
      [ UFunc
        $ Func "quadratic"
          $ Lambda
            ( Sig
              Pure
              [ TypedName (TFlt Immutable) "a"
              , TypedName (TFlt Immutable) "b"
              , TypedName (TFlt Immutable) "c"]
              $ TFunc Pure [TFlt Immutable] $ TFlt Immutable
            )
            [ SExpr
              $ ELambda
                $ Lambda
                  ( Sig
                    Pure
                    [TypedName (TFlt Immutable) "x"]
                    (TFlt Immutable)
                  )
                  [ SExpr
                    $ EAdd
                      ( EMul (EName "a") $ EMul (EName "x") (EName "x") )
                      $ EAdd
                        ( EMul (EName "b") $ EName "x" )
                        $ EName "c"
                  ]
            ]
    ]
    }

  , TestCase
    { nme = "quadratic formula (single root)"
    , src = "singleRoot(Flt a, Flt b, Flt c) -> Flt =>\n\
            \    (-b + math.sqrt(b*b - 4*a*c)) / 2*a"
    , tks = Nothing
    , ast = Just
      [ UFunc
        $ Func "singleRoot"
          $ Lambda
          ( Sig
            Pure
            [ TypedName (TFlt Immutable) "a"
            , TypedName (TFlt Immutable) "b"
            , TypedName (TFlt Immutable) "c" ]
            $ TFlt Immutable
          )
          [ SExpr
            $ EDiv
              ( EAdd
                ( ENegate (EName "b") )
                $ EApply $ Apply
                  ( ESelect $ Select (EName "math") "sqrt" )
                  Pure
                  [ ESub
                    (EMul (EName "b") (EName "b"))
                    (EMul (ELitInt 4) (EMul (EName "a") (EName "c")))
                  ]
              )
              (EMul (ELitInt 2) (EName "a"))
          ]

      ]
    }

  -- TODO: quadratic formula that returns a tuple.
  -- If/when tuples are a thing, I think it may be possible
  -- to generalize:
  --   tuple variables,
  --   tuple construction,
  --   function application
  --   multiple returns
  --
  -- what implications would this have for single-argument function application?
  --
  -- interesting idea in any case, may help to add features and simplify parser.
  --
  -- concerns with this idea: items(index).name could be written items index.name. What would this mean?
  -- would you require haskell style parenthesis like (items index).name ?
  -- I think I may prefer items(index).name
  ]
