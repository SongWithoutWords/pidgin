module TestCases(TestCase(..), testCases) where

import Ast
import qualified Tokens as T
import TypeErrors


data TestCase
  = TestCase
  { name :: String
  , source :: String
  , tokens :: Maybe T.Tokens
  , ast :: Maybe Ast
  , typeErrors :: Maybe TypeErrors }
  deriving(Eq, Show)

type TestCases = [TestCase]


testCases :: TestCases
testCases =
  [ TestCase
    { name = "empty str"
    , source = ""
    , tokens = Just []
    , ast = Just []
    , typeErrors = Just []
    }

  , TestCase
    { name =
      "def pi"
    , source = "$ pi = 3.14159265"
    , tokens = Just [ T.Dollar, T.Name "pi", T.Equal, T.LitFlt 3.14159265 ]
    , ast = Just
      [ UVar
        $ Var
          (TypedName (TInferred Immutable) "pi")
          $ ELitFlt 3.14159265 ]

    , typeErrors = Just []
    }

  , TestCase
    { name = "if expr"
    , source = "$ msg = \"it works!\" if true else \"or not :(\""
    , tokens = Just [ T.Dollar, T.Name "msg", T.Equal, T.LitStr "it works!", T.If, T.True, T.Else, T.LitStr "or not :(" ]
    , ast = Just
      [ UVar
        $ Var
          (TypedName (TInferred Immutable) "msg")
          $ EIf (ELitStr "it works!") (ELitBln True) (ELitStr "or not :(") ]

    , typeErrors = Just []
    }

  , TestCase
    { name = "op expr"
    , source = "$ three = 1 + 2"
    , tokens = Just [ T.Dollar, T.Name "three", T.Equal, T.LitInt 1, T.Plus, T.LitInt 2 ]
    , ast = Just
      [ UVar
        $ Var
          (TypedName (TInferred Immutable) "three")
          $ EAdd (ELitInt 1) (ELitInt 2) ]
    , typeErrors = Just []
    }

  , TestCase
    { name = "def negate inline"
    , source = "negate(Bln b) -> Bln => false if b else true"
    , tokens = Just
      [T.Name "negate", T.LParen, T.TypeBln, T.Name "b", T.RParen, T.ThinArrow, T.TypeBln, T.FatArrow
      , T.False, T.If, T.Name "b", T.Else, T.True]

    , ast = Just
      [ UFunc
        $ Func "negate"
          $ Lambda
            (Sig Pure [TypedName (TBln Immutable) "b"] (TBln Immutable))
            [SExpr $ EIf (ELitBln False) (LExpr $ EName "b") (ELitBln True)] ]
    , typeErrors = Just []
    }

  , TestCase
    { name = "def negate block"
    , source = "negate(Bln b) -> Bln =>\n\
            \    false if b else true"
    , tokens = Just
      [ T.Name "negate", T.LParen, T.TypeBln, T.Name "b", T.RParen, T.ThinArrow, T.TypeBln, T.FatArrow
      , T.Indent
      , T.False, T.If, T.Name "b", T.Else, T.True
      , T.Dedent]

    , ast = Just
      [ UFunc
        $ Func "negate"
          $ Lambda
            (Sig Pure [TypedName (TBln Immutable) "b"] $ TBln Immutable)
            [SExpr $ EIf (ELitBln False) (LExpr $ EName "b") (ELitBln True)] ]

     , typeErrors = Just []
    }

  , TestCase
    { name = "def factorial"

    , source = "factorial(Nat n) -> Nat =>\n\
            \    1 if n <= 0 else n * factorial(n-1)"

    , tokens = Just
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
                (ELesserEq (LExpr $ EName "n") (ELitInt 0))
                (EMul
                    (LExpr $ EName "n")
                    $ LExpr $ EApply
                      (LExpr $ EName "factorial")
                      Pure
                      [ESub (LExpr $ EName "n") (ELitInt 1)]
                )
            ]
      ]

    , typeErrors = Just []
    }

  , TestCase
    { name = "clothing (cascading if exprs inline)"
    , source =
      "clothing(Weather w) -> Clothing =>\n\
      \    rainCoat if w.isRaining else coat if w.isCold else tShirt if w.isSunny else jacket"
    , tokens = Nothing
    , ast = Just
      [ UFunc $ Func "clothing" $ Lambda
        ( Sig Pure [TypedName (TUser Immutable "Weather") "w"] $ TUser Immutable "Clothing" )
        [ SExpr
          $ EIf (LExpr $ EName "rainCoat") (LExpr $ ESelect (LExpr $ EName "w") "isRaining")
          $ EIf (LExpr $ EName "coat") (LExpr $ ESelect (LExpr $ EName "w") "isCold")
          $ EIf (LExpr $ EName "tShirt") (LExpr $ ESelect (LExpr $ EName "w") "isSunny")
          $ LExpr $ EName "jacket"
        ]
      ]

    , typeErrors = Nothing
    }

  , TestCase
    { name = "clothing (cascading if exprs multiline)"
    , source =
      "clothing(Weather w) -> Clothing =>\n\
      \    rainCoat if w.isRaining else\n\
      \    coat if w.isCold else\n\
      \    tShirt if w.isSunny else\n\
      \    jacket"
    , tokens = Nothing
    , ast = Just
      [ UFunc $ Func "clothing" $ Lambda
        ( Sig Pure [TypedName (TUser Immutable "Weather") "w"] $ TUser Immutable "Clothing" )
        [ SExpr
          $ EIf (LExpr $ EName "rainCoat") (LExpr $ ESelect (LExpr $ EName "w") "isRaining")
          $ EIf (LExpr $ EName "coat") (LExpr $ ESelect (LExpr $ EName "w") "isCold")
          $ EIf (LExpr $ EName "tShirt") (LExpr $ ESelect (LExpr $ EName "w") "isSunny")
          $ LExpr $ EName "jacket"
        ]
      ]
    , typeErrors = Nothing
    }

  , TestCase
    { name = "draw widget (imperative-style if)"
    , source = "drawWidget(~@, Nat width, Nat height) -> None =>\n\
            \    $ w = Widget(width, height)\n\
            \    if w.exists\n\
            \        w.draw(~@)\n"
    , tokens = Just
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
                ( TypedName (TInferred Immutable) "w") (ECons "Widget" Pure [LExpr $ EName "width", LExpr $ EName "height"])
            , SIf
              $ Iff
                $ CondBlock
                  ( LExpr $ ESelect (LExpr $ EName "w") "exists" )
                  [ SExpr $ LExpr $ EApply (LExpr $ ESelect (LExpr $ EName "w") "draw") WriteWorld [] ]
            ]
      ]

    , typeErrors = Nothing
    }

  , TestCase
    { name = "quadratic (explicit return type)"
    , source = "quadratic(Flt a, Flt b, Flt c) -> Flt -> Flt =>\n\
            \    (Flt x) -> Flt =>\n\
            \        a*x*x + b*x + c"

    , tokens = Just
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
                      ( EMul (LExpr $ EName "a") $ EMul (LExpr $ EName "x") (LExpr $ EName "x") )
                      $ EAdd
                        ( EMul (LExpr $ EName "b") $ LExpr $ EName "x" )
                        $ LExpr $ EName "c"
                  ]
            ]
      ]
    , typeErrors = Just []
    }

  , TestCase
    { name = "quadratic (implicit return type)"
    , source = "quadratic(Flt a, Flt b, Flt c) =>\n\
            \    (Flt x) =>\n\
            \        a*x*x + b*x + c"

    , tokens = Nothing

    , ast = Just
      [ UFunc
        $ Func "quadratic"
          $ Lambda
            ( Sig
              Pure
              [ TypedName (TFlt Immutable) "a"
              , TypedName (TFlt Immutable) "b"
              , TypedName (TFlt Immutable) "c" ]
              $ TInferred Immutable
            )
            [ SExpr
              $ ELambda
                $ Lambda
                  ( Sig
                    Pure
                    [ TypedName (TFlt Immutable) "x" ]
                    $ TInferred Immutable
                  )
                  [ SExpr
                    $ EAdd
                      ( EMul (LExpr $ EName "a") $ EMul (LExpr $ EName "x") (LExpr $ EName "x") )
                      $ EAdd
                        ( EMul (LExpr $ EName "b") $ LExpr $ EName "x" )
                        $ LExpr $ EName "c"
                  ]
            ]
      ]
    , typeErrors = Just []
    }

  , TestCase
    { name = "quadratic formula (single root)"
    , source = "singleRoot(Flt a, Flt b, Flt c) -> Flt =>\n\
            \    (-b + math.sqrt(b*b - 4*a*c)) / 2*a"
    , tokens = Nothing
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
                ( ENegate (LExpr $ EName "b") )
                $ LExpr $ EApply
                  ( LExpr $ ESelect (LExpr $ EName "math") "sqrt" )
                  Pure
                  [ ESub
                    (EMul (LExpr $ EName "b") (LExpr $ EName "b"))
                    (EMul (ELitInt 4) $ EMul (LExpr $ EName "a") (LExpr $ EName "c"))
                  ]
              )
              (EMul (ELitInt 2) (LExpr $ EName "a"))
          ]

      ]
    , typeErrors = Nothing -- It _will_ have type errors, hold tight! :)
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

  , TestCase
    { name = "Unknown id"
    , source = "$ a = b"
    , tokens = Nothing
    , ast = Nothing
    , typeErrors = Just [UnknownId "b"]
    }
  ]
