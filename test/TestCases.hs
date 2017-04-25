module TestCases(TestCase(..), testCases) where

import Preface

import TestCase
import TestComposer

import Ast
import qualified Tokens as T
import TypeErrors


testCases :: TestCases
testCases =
  [ name "empty str"
    <> source ""
    <> tokens []
    <> ast []
    <> typedAst []
    <> typeErrors []

  , name "def pi"
    <> source "$ pi = 3.14159265"
    <> tokens [ T.Dollar, T.Name "pi", T.Equal, T.LitFlt 3.14159265 ]
    <> ast [ UVar $ Var ((Imut, TInferred), "pi") $ ELitFlt 3.14159265 ]
    <> typeErrors []

  , name "op expr"
    <> source "$ three = 1 + 2"
    <> tokens [ T.Dollar, T.Name "three", T.Equal, T.LitInt 1, T.Plus, T.LitInt 2 ]
    <> ast [ UVar $ Var (Imut & TInferred, "three") $ EAdd (ELitInt 1) (ELitInt 2) ]
    <> typeErrors []

  , name "if expr"
    <> source "$ msg = \"it works!\" if true else \"or not :(\""
    <> tokens [ T.Dollar, T.Name "msg", T.Equal, T.LitStr "it works!", T.If, T.True, T.Else, T.LitStr "or not :(" ]
    <> ast
      [ UVar
        $ Var ((Imut, TInferred), "msg")
          $ EIf (ELitStr "it works!") (ELitBln True) (ELitStr "or not :(") ]
    <> typeErrors []

  , name "negate inline"
    <> source "negate(Bln b) -> Bln => false if b else true"
    <> tokens
      [ T.Name "negate", T.LParen, T.TypeBln, T.Name "b", T.RParen, T.ThinArrow, T.TypeBln, T.FatArrow
      , T.False, T.If, T.Name "b", T.Else, T.True]
    <> ast
      [ UFunc $ Func "negate" $ Lambda (Sig Pure [(Imut, TBln) & "b"] TBln)
        [ SExpr $ EIf (ELitBln False) (EName "b") (ELitBln True) ]
      ]

  , name "negate block"
    <> source
      "negate(Bln b) -> Bln =>\n\
      \    false if b else true"
    <> tokens
      [ T.Name "negate", T.LParen, T.TypeBln, T.Name "b", T.RParen, T.ThinArrow, T.TypeBln, T.FatArrow
      , T.Indent
      , T.False, T.If, T.Name "b", T.Else, T.True
      , T.Dedent]
    <> ast
      [ UFunc $ Func "negate" $ Lambda (Sig Pure [(Imut, TBln) & "b"] TBln)
        [ SExpr $ EIf (ELitBln False) (EName "b") (ELitBln True) ]
      ]

  , name "factorial"
    <> source
      "factorial(Nat n) -> Nat =>\n\
      \    1 if n <= 0 else n * factorial(n-1)"
    <> tokens
      [ T.Name "factorial", T.LParen, T.TypeNat, T.Name "n", T.RParen, T.ThinArrow, T.TypeNat, T.FatArrow
      , T.Indent
      , T.LitInt 1, T.If, T.Name "n", T.LesserEq, T.LitInt 0, T.Else
      , T.Name "n", T.Star, T.Name "factorial", T.LParen, T.Name "n", T.Minus, T.LitInt 1, T.RParen
      , T.Dedent]
    <> ast
      [ UFunc $ Func "factorial" $ Lambda
          ( Sig Pure [(Imut, TNat) & "n"] TNat)
          [ SExpr
            $ EIf
              (ELitInt 1)
              (ELesserEq (EName "n") (ELitInt 0))
              (EMul
                  (EName "n")
                  $ EApply $ EName "factorial" & (Pure & [ESub (EName "n") (ELitInt 1)])
              )
          ]
      ]
    <> typeErrors []

  , name "clothing (cascading if exprs inline)"
    <> source
      "clothing(Weather w) -> Clothing =>\n\
      \    rainCoat if w.isRaining else coat if w.isCold else tShirt if w.isSunny else jacket"
    <> ast
      [ UFunc $ Func "clothing" $ Lambda
        ( Sig Pure [((Imut, TUser "Weather"), "w")] $ TUser "Clothing" )
        [ SExpr
          $ EIf (EName "rainCoat") (ESelect $ EName "w" & "isRaining")
          $ EIf (EName "coat") (ESelect $ EName "w" & "isCold")
          $ EIf (EName "tShirt") (ESelect $ EName "w" & "isSunny")
          $ EName "jacket"
        ]
      ]

  , name "clothing (cascading if exprs multiline)"
    <> source
      "clothing(Weather w) -> Clothing =>\n\
      \    rainCoat if w.isRaining else\n\
      \    coat if w.isCold else\n\
      \    tShirt if w.isSunny else\n\
      \    jacket"
    <> ast
      [ UFunc $ Func "clothing" $ Lambda
        ( Sig Pure [((Imut, TUser "Weather"),  "w")] $ TUser "Clothing" )
        [ SExpr
          $ EIf (EName "rainCoat") (ESelect $ EName "w" & "isRaining")
          $ EIf (EName "coat") (ESelect $ EName "w" & "isCold")
          $ EIf (EName "tShirt") (ESelect $ EName "w" & "isSunny")
          $ EName "jacket"
        ]
      ]

    , name "draw widget (imperative if)"
      <> source
      "drawWidget(~@, Nat width, Nat height) -> None =>\n\
      \    $ w = Widget(width, height)\n\
      \    if w.exists then\n\
      \        w.draw(~@)"

      <> tokens
        [ T.Name "drawWidget"
        , T.LParen, T.Tilde, T.At
        , T.Comma, T.TypeNat, T.Name "width"
        , T.Comma, T.TypeNat, T.Name "height"
        , T.RParen, T.ThinArrow, T.TypeNone, T.FatArrow
        , T.Indent
          , T.Dollar, T.Name "w", T.Equal
            , T.Typename "Widget", T.LParen, T.Name "width", T.Comma, T.Name "height", T.RParen
          , T.Eol
          , T.If, T.Name "w", T.Dot, T.Name "exists", T.Then
          , T.Indent
            , T.Name "w", T.Dot, T.Name "draw", T.LParen, T.Tilde, T.At, T.RParen
          , T.Dedent
        , T.Dedent ]

      <> ast
        [ UFunc
          $ Func "drawWidget"
            $ Lambda
              ( Sig PWrite [(Imut, TNat) & "width", (Imut, TNat) & "height"] TNone )
              [ SVar
                $ Var
                  (Imut & TInferred, "w") (ECons "Widget" $ Pure & [EName "width", EName "height"])
              , SIf
                $ Iff
                  $ CondBlock
                    ( ESelect $ EName "w" & "exists" )
                    [ SExpr $ EApply $ (ESelect $ EName "w" & "draw") & (PWrite, []) ]
              ]
        ]

  , name "quadratic (explicit return types)"
    <> source
      "quadratic(Flt a, Flt b, Flt c) -> Flt -> Flt =>\n\
      \    (Flt x) -> Flt =>\n\
      \        a*x*x + b*x + c"

    <> tokens
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

    <> ast
      [ UFunc
        $ Func "quadratic"
          $ Lambda
            ( Sig Pure [ (Imut & TFlt, "a"), (Imut & TFlt, "b"), (Imut & TFlt, "c")] $ TFunc Pure [TFlt] TFlt )
            [ SExpr
              $ ELambda
                $ Lambda
                  ( Sig
                    Pure
                    [(Imut & TFlt, "x")]
                    TFlt
                  )
                  [ SExpr
                    $ EAdd
                      ( EMul (EName "a") $ EMul (EName "x") (EName "x") )
                      $ EAdd
                        ( EMul ( EName "b") $ EName "x" )
                        $ EName "c"
                  ]
            ]
      ]

  , name "quadratic (implicit return types)"
    <> source
      "quadratic(Flt a, Flt b, Flt c) =>\n\
      \    (Flt x) =>\n\
      \        a*x*x + b*x + c"
    <> ast
      [ UFunc
        $ Func "quadratic"
          $ Lambda
            ( Sig Pure [(Imut & TFlt, "a"), (Imut & TFlt, "b"), (Imut & TFlt, "c")] TInferred )
            [ SExpr
              $ ELambda
                $ Lambda
                  ( Sig Pure [ (Imut & TFlt, "x") ] TInferred )
                  [ SExpr
                    $ EAdd
                      ( EMul (EName "a") $ EMul (EName "x") (EName "x") )
                      $ EAdd
                        ( EMul (EName "b") $ EName "x" )
                        $ EName "c"
                  ]
            ]
      ]

  , name "quadratic formula (single root)"
    <> source
      "singleRoot(Flt a, Flt b, Flt c) -> Flt =>\n\
      \    (-b + math.sqrt(b*b - 4*a*c)) / 2*a"
    <> ast
      [ UFunc
        $ Func "singleRoot"
          $ Lambda
          ( Sig Pure [(Imut & TFlt, "a"), (Imut & TFlt, "b"), (Imut & TFlt, "c")] TFlt )
          [ SExpr
            $ EDiv
              ( EAdd
                ( ENegate (EName "b") )
                $ EApply $
                  (ESelect $ EName "math" & "sqrt" ) &
                  (Pure,
                  [ ESub
                    (EMul (EName "b") (EName "b"))
                    (EMul (ELitInt 4) $ EMul (EName "a") (EName "c"))
                  ])
              )
              (EMul (ELitInt 2) (EName "a"))
          ]
      ]

  -- -- TODO: quadratic formula that returns a tuple.
  -- -- If/when tuples are a thing, I think it may be possible
  -- -- to generalize:
  -- --   tuple variables,
  -- --   tuple construction,
  -- --   function application
  -- --   multiple returns
  -- --
  -- -- what implications would this have for single-argument function application?
  -- --
  -- -- interesting idea in any case, may help to add features and simplify parser.
  -- --
  -- -- concerns with this idea: items(index).name could be written items index.name. What would this mean?
  -- -- would you require haskell style parenthesis like (items index).name ?
  -- -- I think I may prefer items(index).name

  -- These belong at bottom
  ----------------------------------------------------------------------------------------------------------------------
  , source "Bln a = true"
    <> typeErrors []

  , source "Bln a = false"
    <> typeErrors []

  , source "Bln a = 5"
    <> typeErrors [TypeConflict TBln TInt]

  , source "Int a = true"
    <> typeErrors [TypeConflict TInt TBln]

  , source "$ a = b"
    <> typeErrors [UnknownId "a"]

  , source "$ a = true; Bln b = a"
    <> typeErrors []

  , source "Bln a = b; $ b = true"
    <> typeErrors []

  , source "$ a = 5; Bln b = a"
    <> typeErrors [TypeConflict TBln TInt]

  , source "$ a = 5; $ b = a; $ c = b"
    <> typeErrors []

  , source "$ a = 5; $ b = a; Bln c = b"
    <> typeErrors [TypeConflict TBln TInt]

  , source "Bln c = b; $ b = a; $ a = 5"
    <> typeErrors [TypeConflict TBln TInt]
 ]

