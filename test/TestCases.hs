module TestCases(TestCase(..), testCases) where

import TestCase
import TestComposer

import Ast
import qualified Ast1 as A1
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

  , name "indentation lex test (a)"
    <> source
      ".\n\
      \.\n\
      \    .\n\
      \        .\n\
      \    .\n\
      \            .\n\
      \        .\n\
      \            .\n\
      \                .\n\
      \        .\n\
      \"
    <> tokens
      [ T.Dot
      , T.Eol
      , T.Dot
      , T.Indent
          , T.Dot
          , T.Indent
              , T.Dot
          , T.Dedent
          , T.Dot, T.Dot
          , T.Indent
              , T.Dot
              , T.Indent
                  , T.Dot
                  , T.Indent
                      , T.Dot
                  , T.Dedent
              , T.Dedent
              , T.Dot
          , T.Dedent
      , T.Dedent
      ]

  , name "indentation lex test (b)"
    <> source
      ".\n\
      \.\n\
      \\n\
      \    .\n\
      \        .\n\
      \\n\
      \    .\n\
      \            .\n\
      \        .\n\
      \\n\
      \\n\
      \            .\n\
      \                .\n\
      \\n\
      \        .\n\
      \\n\
      \"
    <> tokens
      [ T.Dot
      , T.Eol
      , T.Dot
      , T.Indent
          , T.Dot
          , T.Indent
              , T.Dot
          , T.Dedent
          , T.Dot, T.Dot
          , T.Indent
              , T.Dot
              , T.Indent
                  , T.Dot
                  , T.Indent
                      , T.Dot
                  , T.Dedent
              , T.Dedent
              , T.Dot
          , T.Dedent
      , T.Dedent
      ]

  , name "def pi"
    <> source "$ pi = 3.14159265"
    <> tokens [ T.Dollar, T.Name "pi", T.Equal, T.LitFlt 3.14159265 ]
    <> ast [ UVar $ Var Imut Nothing "pi" $ ELitFlt 3.14159265 ]
    <> typeErrors []

  , name "def pi, def e"
    <> source
      "$ pi = 3.14159265\n\
      \$ e = 2.718281828"
    <> tokens [ T.Dollar, T.Name "pi", T.Equal, T.LitFlt 3.14159265, T.Eol
              , T.Dollar, T.Name "e", T.Equal, T.LitFlt 2.718281828
              ]

  , name "op expr"
    <> source "$ three = 1 + 2"
    <> tokens [ T.Dollar, T.Name "three", T.Equal, T.LitInt 1, T.Plus, T.LitInt 2 ]
    <> ast [ UVar $ Var Imut Nothing "three" $ EAdd (ELitInt 1) (ELitInt 2) ]
    <> typeErrors []

  , name "if expr"
    <> source "$ msg = \"it works!\" if true else \"or not :(\""
    <> tokens [ T.Dollar, T.Name "msg", T.Equal, T.LitStr "it works!", T.If, T.True, T.Else, T.LitStr "or not :(" ]
    <> ast
      [ UVar
        $ Var Imut Nothing "msg"
          $ EIf (ELitStr "it works!") (ELitBln True) (ELitStr "or not :(") ]
    <> typeErrors []

  , name "negate inline"
    <> source "negate(Bln b) -> Bln => false if b else true"
    <> tokens
      [ T.Name "negate", T.LParen, T.TypeBln, T.Name "b", T.RParen, T.ThinArrow, T.TypeBln, T.FatArrow
      , T.False, T.If, T.Name "b", T.Else, T.True]
    <> ast
      [ UFunc $ Func "negate" $ Lambda (Sig Pure [Param Imut TBln "b"] $ Just TBln)
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
      [ UFunc $ Func "negate" $ Lambda (Sig Pure [Param Imut TBln "b"] $ Just TBln)
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
          ( Sig Pure [Param Imut TNat "n"] $ Just TNat)
          [ SExpr
            $ EIf
              (ELitInt 1)
              (ELesserEq (EName "n") (ELitInt 0))
              (EMul
                  (EName "n")
                  $ EApp $ App (EName "factorial") $ Args Pure [ESub (EName "n") (ELitInt 1)]
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
        ( Sig Pure [Param Imut (TUser "Weather") "w"] $ Just $ TUser "Clothing" )
        [ SExpr
          $ EIf (EName "rainCoat") (ESelect $ Select (EName "w") "isRaining")
          $ EIf (EName "coat") (ESelect $ Select (EName "w") "isCold")
          $ EIf (EName "tShirt") (ESelect $ Select (EName "w") "isSunny")
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
        ( Sig Pure [Param Imut (TUser "Weather") "w"] $ Just $ TUser "Clothing" )
        [ SExpr
          $ EIf (EName "rainCoat") (ESelect $ Select (EName "w") "isRaining")
          $ EIf (EName "coat") (ESelect $ Select (EName "w") "isCold")
          $ EIf (EName "tShirt") (ESelect $ Select (EName "w") "isSunny")
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
              ( Sig PWrite [Param Imut TNat "width", Param Imut TNat "height"] $ Just TNone )
              [ SVar
                $ Var Imut Nothing "w" (ECons "Widget" $ Args Pure [EName "width", EName "height"])
              , SIf
                $ Iff
                  $ CondBlock
                    ( ESelect $ Select (EName "w") "exists" )
                    [ SExpr $ EApp $ App (ESelect $ Select (EName "w") "draw") $ Args PWrite [] ]
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
            ( Sig Pure [Param Imut TFlt "a", Param Imut TFlt "b", Param Imut TFlt "c"]
              $ Just $ TFunc Pure [TFlt] $ Just TFlt
            )
            [ SExpr
              $ ELambda
                $ Lambda
                  ( Sig
                    Pure
                    [Param Imut TFlt "x"]
                    $ Just TFlt
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
            ( Sig Pure [Param Imut TFlt "a", Param Imut TFlt "b", Param Imut TFlt "c"] Nothing)
            [ SExpr
              $ ELambda
                $ Lambda
                  ( Sig Pure [Param Imut TFlt "x"] Nothing)
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
          ( Sig Pure [Param Imut TFlt "a", Param Imut TFlt "b", Param Imut TFlt "c"] $ Just TFlt )
          [ SExpr
            $ EDiv
              ( EAdd
                ( ENegate (EName "b") )
                $ EApp $ App
                  (ESelect $ Select (EName "math") "sqrt" )
                  $ Args
                    Pure
                    [ ESub
                      (EMul (EName "b") (EName "b"))
                      (EMul (ELitInt 4) $ EMul (EName "a") (EName "c"))
                    ]
              )
              (EMul (ELitInt 2) (EName "a"))
          ]
      ]

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


  -- TypeCheck tests
  ----------------------------------------------------------------------------------------------------------------------
  , source "Bln a = true"
    <> typeErrors []

  , source "Bln a = false"
    <> typeErrors []

  , source "Bln a = 5"
    <> typeErrors [TypeConflict {expected = TBln, received = TInt}]

  , source "Int a = true"
    <> typedAst [("a", A1.UVar $ A1.Var Imut (Just TInt) $ ELitBln True)]
    <> typeErrors [TypeConflict {expected = TInt, received = TBln}]

  , source "$ a = b"
    <> typedAst [("a", A1.UVar $ A1.Var Imut Nothing $ EName "b")]
    <> typeErrors [UnknownId "b"]

  -- TODO: a proper error type and error handling for recursive definitions
  , source "$ a = a"
    <> typedAst [("a", A1.UVar $ A1.Var Imut Nothing $ EName "a")]
    <> typeErrors []

  , source "$ a = b; $ b = a"
    <> typedAst [ ("a", A1.UVar $ A1.Var Imut Nothing $ EName "b")
                , ("b", A1.UVar $ A1.Var Imut Nothing $ EName "a")]
    <> typeErrors []

  , source "$ a = true; $ b = a"
    <> typedAst [ ("a", A1.UVar $ A1.Var Imut (Just TBln) $ ELitBln True)
                , ("b", A1.UVar $ A1.Var Imut (Just TBln) $ EName "a")]
    <> typeErrors []

  , source "$ a = b; $ b = true"
    <> typedAst [ ("a", A1.UVar $ A1.Var Imut (Just TBln) $ EName "b")
                , ("b", A1.UVar $ A1.Var Imut (Just TBln) $ ELitBln True)]
    <> typeErrors []

  , source "$ a = 5; Bln b = a"
    <> typeErrors [TypeConflict {expected = TBln, received = TInt}]

  , source "$ a = 5; $ b = a; $ c = b"
    <> typedAst [ ("a", A1.UVar $ A1.Var Imut (Just TInt) $ ELitInt 5)
                , ("b", A1.UVar $ A1.Var Imut (Just TInt) $ EName "a")
                , ("c", A1.UVar $ A1.Var Imut (Just TInt) $ EName "b")]
    <> typeErrors []

  , source "$ a = 5; $ b = a; Bln c = b"
    <> typedAst [ ("a", A1.UVar $ A1.Var Imut (Just TInt) $ ELitInt 5)
                , ("b", A1.UVar $ A1.Var Imut (Just TInt) $ EName "a")
                , ("c", A1.UVar $ A1.Var Imut (Just TBln) $ EName "b")]
    <> typeErrors [TypeConflict {expected = TBln, received = TInt}]

  , source "Bln a = b; $ b = c; $ c = 5"
    <> typedAst [ ("a", A1.UVar $ A1.Var Imut (Just TBln) $ EName "b")
                , ("b", A1.UVar $ A1.Var Imut (Just TInt) $ EName "c")
                , ("c", A1.UVar $ A1.Var Imut (Just TInt) $ ELitInt 5)]
    <> typeErrors [TypeConflict {expected = TBln, received = TInt}]


  -- TypeCheck operator tests
  , source "$ a = 3 + 7"
    <> typedAst [ ("a", A1.UVar $ A1.Var Imut (Just TInt) $ EAdd (ELitInt 3) (ELitInt 7))]
    <> typeErrors []

  , source "$ a = b + c; $ b = 3; $ c = 7"
    <> typedAst [ ("a", A1.UVar $ A1.Var Imut (Just TInt) $ EAdd (EName "b") (EName "c"))
                , ("b", A1.UVar $ A1.Var Imut (Just TInt) $ ELitInt 3)
                , ("c", A1.UVar $ A1.Var Imut (Just TInt) $ ELitInt 7)]
    <> typeErrors []

  , source "$ a = 1 if true else 0"
    <> typedAst [("a", A1.UVar $ A1.Var Imut (Just TInt) $ EIf (ELitInt 1) (ELitBln True) (ELitInt 0))]
    <> typeErrors []

  , source "$ a = 1 if \"true\" else 0"
    <> typedAst [("a", A1.UVar $ A1.Var Imut (Just TInt) $ EIf (ELitInt 1) (ELitStr "true") (ELitInt 0))]
    <> typeErrors [TypeConflict {expected = TBln, received = TStr}]

  , source "$ a = 1 if true else \"zero\""
    <> typedAst [("a", A1.UVar $ A1.Var Imut (Just TInt) $ EIf (ELitInt 1) (ELitBln True) (ELitStr "zero"))]
    <> typeErrors [TypeConflict {expected = TInt, received = TStr}]


  -- TypeCheck function tests
  , source "$ a = 5(1)"
    <> typeErrors [NonApplicable TInt]
  
  , name "one explicit"
    <> source
      "one() -> Int => 1\n\
      \$ a = one()"
    <> typedAst
      [ ( "one", A1.UFunc $
        Lambda
          ( Sig Pure [] $ Just TInt)
          [ SExpr $ ELitInt 1]
        )
      , ( "a", A1.UVar $ A1.Var Imut (Just TInt) $ EApp $ App (EName "one") $ Args Pure [])
      ]
    <> typeErrors []

  , name "one explicit, wrong return type"
    <> source
      "one() -> Int => \"one\""
    <> typeErrors [TypeConflict {expected = TInt, received = TStr}]

  , name "one implicit, wrong num args"
    <> source
      "one() => 1\n\
      \$ a = one(1)"
    <> typeErrors [ArgCount {acExpected = 0, acReceived = 1}]

  , name "inc explicit"
    <> source
      "inc(Int x) -> Int => x + 1\n\
      \$ a = inc(1)"
    <> typedAst
      [ ( "inc", A1.UFunc $
        Lambda
          ( Sig Pure [Param Imut TInt "x"] $ Just TInt )
          [ SExpr $ EAdd (EName "x") $ ELitInt 1 ]
        )
      , ( "a", A1.UVar $ A1.Var Imut (Just TInt) $ EApp $ App (EName "inc") $ Args Pure [ELitInt 1] )
      ]
    <> typeErrors []

  , name "inc explicit, nested calls"
    <> source
      "inc(Int x) -> Int => x + 1\n\
      \$ a = inc(inc(1))"
    <> typedAst
      [ ( "inc", A1.UFunc $
        Lambda
          ( Sig Pure [Param Imut TInt "x"] (Just TInt) )
          [ SExpr $ EAdd (EName "x") $ ELitInt 1]
        )
      , ( "a", A1.UVar $ A1.Var Imut (Just TInt) $
          EApp $ App (EName "inc") $ Args Pure [EApp $ App (EName "inc") $ Args Pure [ELitInt 1]]
        )
      ]
    <> typeErrors []

  , name "inc explicit, wrong return type"
    <> source
      "inc(Int x) -> Int => \"one\""
    <> typeErrors [TypeConflict {expected = TInt, received = TStr}]

  , name "inc explicit, wrong num of args (a)"
    <> source
      "inc(Int x) -> Int => x + 1\n\
      \$ a = inc()"
    <> typeErrors []

  , name "inc explicit, wrong num of args (b)"
    <> source
      "inc(Int x) -> Int => x + 1\n\
      \$ a = inc(1, 2)"
    <> typeErrors []

  , name "inc implicit"
    <> source
      "inc(Int x) => x + 1\n\
      \$ a = inc(1)"
    <> typedAst
      [ ("inc", A1.UFunc $
        Lambda
          ( Sig Pure [Param Imut TInt "x"] $ Just TInt )
          [ SExpr $ EAdd (EName "x") $ ELitInt 1 ]
        )
      , ("a", A1.UVar $ A1.Var Imut (Just TInt) $ EApp $ App (EName "inc") $ Args Pure [ELitInt 1])
      ]
    <> typeErrors []

  , name "inc implicit, wrong arg type"
    <> source
      "inc(Int x) => x + 1\n\
      \$ a = inc(\"one\")"
    <> typedAst
      [ ("inc", A1.UFunc $
        Lambda
          ( Sig Pure [Param Imut TInt "x"] $ Just TInt )
          [ SExpr $ EAdd (EName "x") $ ELitInt 1 ]
        )
      , ("a", A1.UVar $ A1.Var Imut (Just TInt) $ EApp $ App (EName "inc") $ Args Pure [ELitStr "one"])
      ]
    <> typeErrors [TypeConflict {expected = TInt, received = TStr}]

  , name "inc implicit, local var"
    <> source
      "inc(Int x) =>\n\
      \    $ one = 1\n\
      \    x + one\n\
      \\n\
      \$ a = inc(1)"
    <> typedAst
      [ ( "inc", A1.UFunc $
        Lambda
          ( Sig Pure [Param Imut TInt "x"] (Just TInt) )
          [ SVar $ Var Imut (Just TInt) "one" (ELitInt 1)
          , SExpr $ EAdd (EName "x") (EName "one")
          ]
        )
      , ( "a", A1.UVar $ A1.Var Imut (Just TInt) $ EApp $ App (EName "inc") $ Args Pure [ELitInt 1])
      ]
    <> typeErrors []
 ]

