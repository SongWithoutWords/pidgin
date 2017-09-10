module TestCases(TestCase(..), testCases) where

import TestCase
import TestComposer

import Ast
import AstBuilderU
import AstBuilderT
import qualified Tokens as T


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
    <> ast [ UVar $ VarLu Imut Nothing "pi" $ eValFlt 3.14159265 ]
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
    <> ast [ UVar $ VarLu Imut Nothing "three" $ eBinOp Add (eValInt 1) (eValInt 2) ]
    <> typeErrors []

  , name "if expr"
    <> source "$ msg = \"it works!\" if true else \"or not :(\""
    <> tokens [ T.Dollar, T.Name "msg", T.Equal, T.LitStr "it works!", T.If, T.True, T.Else, T.LitStr "or not :(" ]
    <> ast
      [ UVar
        $ VarLu Imut Nothing "msg"
          $ eIf (eValStr "it works!") (eValBln True) (eValStr "or not :(") ]
    <> typeErrors []

  , name "negate inline"
    <> source "negate(Bln b) -> Bln => false if b else true"
    <> tokens
      [ T.Name "negate", T.LParen, T.TypeBln, T.Name "b", T.RParen, T.ThinArrow, T.TypeBln, T.FatArrow
      , T.False, T.If, T.Name "b", T.Else, T.True]
    <> ast
      [ UFuncL $ Func "negate" $ Lambda (SigU Pure [Param Imut TBln "b"] $ Just TBln) ImplicitRet
        [ SExpr $ eIf (eValBln False) (eName "b") (eValBln True) ]
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
      [ UFuncL $ Func "negate" $ Lambda (SigU Pure [Param Imut TBln "b"] $ Just TBln) ImplicitRet
        [ SExpr $ eIf (eValBln False) (eName "b") (eValBln True) ]
      ]

  -- TODO: support dependently typed natural numbers
  , name "factorial"
    <> source
      "factorial(Int n) -> Int =>\n\
      \    1 if n <= 0 else n * factorial(n-1)"
    <> tokens
      [ T.Name "factorial", T.LParen, T.TypeInt, T.Name "n", T.RParen, T.ThinArrow, T.TypeInt, T.FatArrow
      , T.Indent
      , T.LitInt 1, T.If, T.Name "n", T.LesserEq, T.LitInt 0, T.Else
      , T.Name "n", T.Star, T.Name "factorial", T.LParen, T.Name "n", T.Minus, T.LitInt 1, T.RParen
      , T.Dedent]
    <> ast
      [ UFuncL $ Func "factorial" $ Lambda
          ( SigU Pure [Param Imut TInt "n"] $ Just TInt) ImplicitRet
          [ SExpr
            $ eIf
              (eValInt 1)
              (eBinOp LesserEq (eName "n") (eValInt 0))
              (eBinOp Mul
                  (eName "n")
                  $ eApp (eName "factorial") $ Args Pure [eBinOp Sub (eName "n") (eValInt 1)]
              )
          ]
      ]
    <> typeErrors []

  , name "clothing (cascading if exprs inline)"
    <> source
      "clothing(Weather w) -> Clothing =>\n\
      \    rainCoat if w.isRaining else coat if w.isCold else tShirt if w.isSunny else jacket"
    <> ast
      [ UFuncL $ Func "clothing" $ Lambda
        ( SigU Pure [Param Imut (TUser "Weather") "w"] $ Just $ TUser "Clothing" ) ImplicitRet
        [ SExpr
          $ eIf (eName "rainCoat") (eSelect (eName "w") "isRaining")
          $ eIf (eName "coat") (eSelect (eName "w") "isCold")
          $ eIf (eName "tShirt") (eSelect (eName "w") "isSunny")
          $ eName "jacket"
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
      [ UFuncL $ Func "clothing" $ Lambda
        ( SigU Pure [Param Imut (TUser "Weather") "w"] $ Just $ TUser "Clothing" ) ImplicitRet
        [ SExpr
          $ eIf (eName "rainCoat") (eSelect (eName "w") "isRaining")
          $ eIf (eName "coat") (eSelect (eName "w") "isCold")
          $ eIf (eName "tShirt") (eSelect (eName "w") "isSunny")
          $ eName "jacket"
        ]
      ]

    , name "draw widget (imperative if)"
      <> source
      "drawWidget(~@, Nat width, Nat height):\n\
      \    $ w = Widget(width, height)\n\
      \    if w.exists then\n\
      \        w.draw(~@)"

      <> tokens
        [ T.Name "drawWidget"
        , T.LParen, T.Tilde, T.At
        , T.Comma, T.TypeNat, T.Name "width"
        , T.Comma, T.TypeNat, T.Name "height"
        , T.RParen, T.Colon
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
        [ UFuncL
          $ Func "drawWidget"
            $ Lambda
              ( SigU PWrite [Param Imut TNat "width", Param Imut TNat "height"] $ Nothing )
              ExplicitRet
              [ SVar
                $ VarLu Imut Nothing "w" (eCons "Widget" $ Args Pure [eName "width", eName "height"])
              , SIf
                $ Iff
                  $ CondBlock
                    ( eSelect (eName "w") "exists" )
                    [ SExpr $ eApp (eSelect (eName "w") "draw") $ Args PWrite [] ]
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
      [ UFuncL
        $ Func "quadratic"
          $ Lambda
            ( SigU Pure [Param Imut TFlt "a", Param Imut TFlt "b", Param Imut TFlt "c"]
              $ Just $ TFunc Pure [TFlt] $ TFlt
            ) ImplicitRet
            [ SExpr
              $ eLambda
                  ( SigU Pure [Param Imut TFlt "x"] $ Just TFlt ) ImplicitRet
                  [ SExpr
                    $ eBinOp Add
                      ( eBinOp Mul (eName "a") $ eBinOp Mul (eName "x") (eName "x") )
                      $ eBinOp Add
                        ( eBinOp Mul ( eName "b") $ eName "x" )
                        $ eName "c"
                  ]
            ]
      ]

  , name "quadratic (implicit return types)"
    <> source
      "quadratic(Flt a, Flt b, Flt c) =>\n\
      \    (Flt x) =>\n\
      \        a*x*x + b*x + c"
    <> ast
      [ UFuncL
        $ Func "quadratic"
          $ Lambda
            ( SigU Pure [Param Imut TFlt "a", Param Imut TFlt "b", Param Imut TFlt "c"] Nothing) ImplicitRet
            [ SExpr
              $ eLambda
                ( SigU Pure [Param Imut TFlt "x"] Nothing) ImplicitRet
                [ SExpr
                  $ eBinOp Add
                    ( eBinOp Mul (eName "a") $ eBinOp Mul (eName "x") (eName "x") )
                    $ eBinOp Add
                      ( eBinOp Mul (eName "b") $ eName "x" )
                      $ eName "c"
                ]
            ]
      ]

  , name "quadratic formula (single root)"
    <> source
      "singleRoot(Flt a, Flt b, Flt c) -> Flt =>\n\
      \    (-b + math.sqrt(b*b - 4*a*c)) / 2*a"
    <> ast
      [ UFuncL
        $ Func "singleRoot"
          $ Lambda
          ( SigU Pure [Param Imut TFlt "a", Param Imut TFlt "b", Param Imut TFlt "c"] $ Just TFlt ) ImplicitRet
          [ SExpr
            $ eBinOp Div
              ( eBinOp Add
                ( eUnOp Neg (eName "b") )
                $ eApp
                  (eSelect (eName "math") "sqrt" )
                  $ Args
                    Pure
                    [ eBinOp Sub
                      (eBinOp Mul (eName "b") (eName "b"))
                      (eBinOp Mul (eValInt 4) $ eBinOp Mul (eName "a") (eName "c"))
                    ]
              )
              (eBinOp Mul (eValInt 2) (eName "a"))
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
    <> typeErrors [TypeConflict {typeRequired = TBln, typeFound = TInt}]

  , source "Int a = true"
    <> typedAst [("a", UVar $ VarMc Imut TInt $ ExprT TBln $ EValBln True)]
    <> typeErrors [TypeConflict {typeRequired = TInt, typeFound = TBln}]

  , source "$ a = b"
    <> typedAst [("a", UVar $ VarMc Imut (TError $ UnknownId "b")
                   $ ExprT (TError $ UnknownId "b") $ EName "b")]
    <> typeErrors [UnknownId "b"]

  -- TODO: a proper error type and error handling for recursive definitions
  , source "$ a = a"
    <> typedAst [("a", UVar $ VarMc Imut (TError $ recursiveDefinition ["a"])
                   $ ExprT (TError $ recursiveDefinition ["a"]) $ EName "a")]

    <> typeErrors [recursiveDefinition ["a"]]

  , source "$ a = b; $ b = a"
    <> let recErr = TError $ recursiveDefinition ["a", "b"]
       in typedAst [ ("a", UVar $ VarMc Imut recErr $ ExprT recErr $ EName "b")
                   , ("b", UVar $ VarMc Imut recErr $ ExprT recErr $ EName "a")]
    <> typeErrors [recursiveDefinition ["a", "b"]]


  , source "$ a = b; $ b = b"
    <> let recDef = recursiveDefinition ["b"]
           bType = TError recDef
       in typedAst
          [ ("a", UVar $ VarMc Imut (TError Propagated) $ ExprT (TError Propagated) $ EName "b")
          , ("b", UVar $ VarMc Imut bType $ ExprT bType $ EName "b")]

    <> typeErrors [recursiveDefinition ["b"]]


  , source "$ a = b; $ b = c; $ c = a"

    <> let cycleT = TError $ recursiveDefinition ["a", "b", "c"] in
       typedAst [ ("a", UVar $ VarMc Imut cycleT $ ExprT cycleT $ EName "b")
                , ("b", UVar $ VarMc Imut cycleT $ ExprT cycleT $ EName "c")
                , ("c", UVar $ VarMc Imut cycleT $ ExprT cycleT $ EName "a")]

    <> typeErrors [recursiveDefinition ["a", "b", "c"]]


  , source "$ a = b; $ b = c; $ c = b"
    <> let cycle = recursiveDefinition ["b", "c"]
           cycleT = TError cycle in
       typedAst [ ("a", UVar $ VarMc Imut (TError Propagated) $ ExprT (TError Propagated) $ EName "b")
                , ("b", UVar $ VarMc Imut cycleT $ ExprT cycleT $ EName "c")
                , ("c", UVar $ VarMc Imut cycleT $ ExprT cycleT $ EName "b")]
    <> typeErrors [recursiveDefinition ["b", "c"]]


  , source "$ a = 1; $ b = b + a"

    <> let bType = TError $ recursiveDefinition ["b"]
       in typedAst [ ("a", UVar $ VarMc Imut TInt $ ExprT TInt $ EValInt 1)
                  , ("b", UVar $ VarMc Imut (TError Propagated) $ ExprT (TError Propagated)
                      $ EBinOp Add
                        (ExprT (TError $ recursiveDefinition ["b"]) $ EName "b")
                        (ExprT TInt $ EName "a"))]

    <> typeErrors [recursiveDefinition ["b"]]


  , source "$ a = true; $ a = false; $ b = a"
    <> typeErrors [CompetingDefinitions]

  , source "$ a = true; a() => true; $ b = a"
    <> typeErrors [CompetingDefinitions]

  , source "$ a = true; $ b = a"
    <> typedAst [ ("a", UVar $ VarMc Imut TBln $ tValBln True)
                , ("b", UVar $ VarMc Imut TBln $ tName TBln "a")]
    <> typeErrors []

  , source "$ a = b; $ b = true"
    <> typedAst [ ("a", UVar $ VarMc Imut TBln $ tName TBln "b")
                , ("b", UVar $ VarMc Imut TBln $ tValBln True)]
    <> typeErrors []

  , source "$ a = 5; Bln b = a"
    <> typeErrors [TypeConflict {typeRequired = TBln, typeFound = TInt}]

  , source "$ a = 5; $ b = a; $ c = b"
    <> typedAst [ ("a", UVar $ VarMc Imut TInt $ tValInt 5)
                , ("b", UVar $ VarMc Imut TInt $ tName TInt "a")
                , ("c", UVar $ VarMc Imut TInt $ tName TInt "b")]
    <> typeErrors []

  , source "$ a = 5; $ b = a; Bln c = b"
    <> typedAst [ ("a", UVar $ VarMc Imut TInt $ tValInt 5)
                , ("b", UVar $ VarMc Imut TInt $ tName TInt "a")
                , ("c", UVar $ VarMc Imut TBln $ tName TInt "b")]
    <> typeErrors [TypeConflict {typeRequired = TBln, typeFound = TInt}]

  , source "Bln a = b; $ b = c; $ c = 5"
    <> typedAst [ ("a", UVar $ VarMc Imut TBln $ tName TInt "b")
                , ("b", UVar $ VarMc Imut TInt $ tName TInt "c")
                , ("c", UVar $ VarMc Imut TInt $ tValInt 5)]
    <> typeErrors [TypeConflict {typeRequired = TBln, typeFound = TInt}]


  -- TypeCheck operator tests
  , source "$ a = 3 + 7"
    <> typedAst [ ("a", UVar $ VarMc Imut TInt $ tBinOp TInt Add (tValInt 3) (tValInt 7))]
    <> typeErrors []

  , source "$ a = b + c; $ b = 3; $ c = 7"
    <> typedAst [ ("a", UVar $ VarMc Imut TInt $ tBinOp TInt Add (tName TInt "b") (tName TInt "c"))
                , ("b", UVar $ VarMc Imut TInt $ tValInt 3)
                , ("c", UVar $ VarMc Imut TInt $ tValInt 7)]
    <> typeErrors []

  , source "$ a = 1 if true else 0"
    <> typedAst [("a", UVar $ VarMc Imut TInt $ tIf TInt (tValInt 1) (tValBln True) (tValInt 0))]
    <> typeErrors []

  , source "$ a = 1 if \"true\" else 0"
    <> typedAst [("a", UVar $ VarMc Imut TInt $ tIf TInt (tValInt 1) (tValStr "true") (tValInt 0))]
    <> typeErrors [TypeConflict {typeRequired = TBln, typeFound = TStr}]

  , source "$ a = 1 if true else \"zero\""
    <> typedAst [("a", UVar $ VarMc Imut TInt $ tIf TInt (tValInt 1) (tValBln True) (tValStr "zero"))]
    <> typeErrors [TypeConflict {typeRequired = TInt, typeFound = TStr}]


  -- TypeCheck function tests
  , source "$ a = 5(1)"
    <> typeErrors [NonApplicable TInt]

  , source "$ a = (3 + 2)(1)"
    <> typeErrors [NonApplicable TInt]

  , name "one explicit"
    <> source
      "one() -> Int => 1\n\
      \$ a = one()"
    <> typedAst
      [ ( "one", UFuncM $
        Lambda
          ( SigC Pure [] TInt) ImplicitRet
          [ SExpr $ tValInt 1]
        )
      , ( "a", UVar $ VarMc Imut TInt $ tApp TInt (tName (TFunc Pure [] TInt) "one") $ Args Pure [])
      ]
    <> typeErrors []

  , name "one explicit, wrong return type"
    <> source
      "one() -> Int => \"one\""
    <> typeErrors [TypeConflict {typeRequired = TInt, typeFound = TStr}]

  , name "one implicit, wrong num args"
    <> source
      "one() => 1\n\
      \$ a = one(1)"
    <> typeErrors [WrongNumArgs {numArgsRequired = 0, numArgsFound = 1}]

  , name "inc explicit"
    <> source
      "inc(Int x) -> Int => x + 1\n\
      \$ a = inc(1)"
    <> typedAst
      [ ( "inc", UFuncM $
        Lambda
          ( SigC Pure [Param Imut TInt "x"] TInt ) ImplicitRet
          [ SExpr $ tBinOp TInt Add (tName TInt "x") $ tValInt 1 ]
        )
      , ( "a", UVar $ VarMc Imut TInt $
          tApp TInt (tName (TFunc Pure [TInt] TInt) "inc") $ Args Pure [tValInt 1] )
      ]
    <> typeErrors []

  , name "inc explicit, nested calls"
    <> source
      "inc(Int x) -> Int => x + 1\n\
      \$ a = inc(inc(1))"
    <> let incType = TFunc Pure [TInt] TInt in
      typedAst
      [ ( "inc", UFuncM $
        Lambda
          ( SigC Pure [Param Imut TInt "x"] TInt ) ImplicitRet
          [ SExpr $ tBinOp TInt Add (tName TInt "x") $ tValInt 1]
        )
      , ( "a", UVar $ VarMc Imut TInt $
          tApp TInt (tName incType "inc") $ Args Pure
            [tApp TInt (tName incType "inc") $ Args Pure [tValInt 1]]
        )
      ]
    <> typeErrors []

  , name "inc explicit, wrong return type"
    <> source
      "inc(Int x) -> Int => \"one\""
    <> typeErrors [TypeConflict {typeRequired = TInt, typeFound = TStr}]

  , name "inc explicit, wrong num args (a)"
    <> source
      "inc(Int x) -> Int => x + 1\n\
      \$ a = inc()"
    <> typeErrors [WrongNumArgs {numArgsRequired = 1, numArgsFound = 0}]

  , name "inc explicit, wrong num args (b)"
    <> source
      "inc(Int x) -> Int => x + 1\n\
      \$ a = inc(1, 2)"
    <> typeErrors [WrongNumArgs {numArgsRequired = 1, numArgsFound = 2}]

  , name "inc implicit"
    <> source
      "inc(Int x) => x + 1\n\
      \$ a = inc(1)"
    <> typedAst
      [ ("inc", UFuncM $
        Lambda
          ( SigC Pure [Param Imut TInt "x"] TInt ) ImplicitRet
          [ SExpr $ tBinOp TInt Add (tName TInt "x") $ tValInt 1 ]
        )
      , ("a", UVar $ VarMc Imut TInt $ tApp TInt (tName (TFunc Pure [TInt] TInt) "inc") $ Args Pure [tValInt 1])
      ]
    <> typeErrors []

  , name "inc implicit, wrong arg type"
    <> source
      "inc(Int x) => x + 1\n\
      \$ a = inc(\"one\")"
    <> typedAst
      [ ("inc", UFuncM $
        Lambda
          ( SigC Pure [Param Imut TInt "x"] TInt ) ImplicitRet
          [ SExpr $ tBinOp TInt Add (tName TInt "x") $ tValInt 1 ]
        )
      , ("a", UVar $ VarMc Imut TInt
          $ tApp TInt (tName (TFunc Pure [TInt] TInt) "inc") $ Args Pure [tValStr "one"])
      ]
    <> typeErrors [TypeConflict {typeRequired = TInt, typeFound = TStr}]

  , name "inc implicit, local var"
    <> source
      "inc(Int x) =>\n\
      \    $ one = 1\n\
      \    x + one\n\
      \\n\
      \$ a = inc(1)"
    <> typedAst
      [ ( "inc", UFuncM $
        Lambda
          ( SigC Pure [Param Imut TInt "x"] TInt ) ImplicitRet
          [ SVar $ VarLc Imut TInt "one" (tValInt 1)
          , SExpr $ tBinOp TInt Add (tName TInt "x") (tName TInt "one")
          ]
        )
      , ( "a", UVar $ VarMc Imut TInt
          $ tApp TInt (tName (TFunc Pure [TInt] TInt) "inc") $ Args Pure [tValInt 1])
      ]
    <> typeErrors []
 ]

