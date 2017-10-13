module TestCases(TestCase(..), testCases) where

import TestCase
import TestComposer

import Ast
import Ast0Builder
import Ast2Builder
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
    <> ast [ namedU0Var "pi" Imut Nothing $ e0ValFlt 3.14159265 ]
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
    <> ast [ namedU0Var "three" Imut Nothing $ e0BinOp Add (e0ValInt 1) (e0ValInt 2) ]
    <> typeErrors []

  , name "if expr"
    <> source "$ msg = \"it works!\" if true else \"or not :(\""
    <> tokens [ T.Dollar, T.Name "msg", T.Equal, T.LitStr "it works!", T.If, T.True, T.Else, T.LitStr "or not :(" ]
    <> ast
      [ namedU0Var "msg" Imut Nothing
        $ e0If (e0ValStr "it works!") (e0ValBln True) (e0ValStr "or not :(") ]
    <> typeErrors []

  , name "negate inline"
    <> source "negate(Bln b) -> Bln => false if b else true"
    <> tokens
      [ T.Name "negate", T.LParen, T.TypeBln, T.Name "b", T.RParen, T.ThinArrow, T.TypeBln, T.FatArrow
      , T.False, T.If, T.Name "b", T.Else, T.True]
    <> ast
      [ namedU0Func "negate" (Sig0 Pure [Param Imut TBln "b"] $ Just TBln) ImplicitRet
        $ Block0 [ SExpr $ e0If (e0ValBln False) (e0Name "b") (e0ValBln True) ]
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
      [ namedU0Func "negate" (Sig0 Pure [Param Imut TBln "b"] $ Just TBln) ImplicitRet
        $ Block0 [ SExpr $ e0If (e0ValBln False) (e0Name "b") (e0ValBln True) ]
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
      [ namedU0Func "factorial" ( Sig0 Pure [Param Imut TInt "n"] $ Just TInt) ImplicitRet $ Block0
        [ SExpr
          $ e0If
            (e0ValInt 1)
            (e0BinOp (Cmp LesserEq) (e0Name "n") (e0ValInt 0))
            (e0BinOp Mul
                (e0Name "n")
                $ e0App (e0Name "factorial") $ Args Pure [e0BinOp Sub (e0Name "n") (e0ValInt 1)]
            )
        ]
      ]
    <> typeErrors []

  , name "clothing (cascading if exprs inline)"
    <> source
      "clothing(Weather w) -> Clothing =>\n\
      \    rainCoat if w.isRaining else coat if w.isCold else tShirt if w.isSunny else jacket"
    <> ast
      [ namedU0Func "clothing"
        ( Sig0 Pure [Param Imut (TUser "Weather") "w"] $ Just $ TUser "Clothing" ) ImplicitRet $ Block0
        [ SExpr
          $ e0If (e0Name "rainCoat") (e0Select (e0Name "w") "isRaining")
          $ e0If (e0Name "coat") (e0Select (e0Name "w") "isCold")
          $ e0If (e0Name "tShirt") (e0Select (e0Name "w") "isSunny")
          $ e0Name "jacket"
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
      [ namedU0Func "clothing"
        ( Sig0 Pure [Param Imut (TUser "Weather") "w"] $ Just $ TUser "Clothing" ) ImplicitRet $ Block0
        [ SExpr
          $ e0If (e0Name "rainCoat") (e0Select (e0Name "w") "isRaining")
          $ e0If (e0Name "coat") (e0Select (e0Name "w") "isCold")
          $ e0If (e0Name "tShirt") (e0Select (e0Name "w") "isSunny")
          $ e0Name "jacket"
        ]
      ]

    , name "draw widget (imperative if)"
      <> source
      "drawWidget(~@, Nat width, Nat height):\n\
      \    $ w = Widget(width, height)\n\
      \    if w.exists:\n\
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
          , T.If, T.Name "w", T.Dot, T.Name "exists", T.Colon
          , T.Indent
            , T.Name "w", T.Dot, T.Name "draw", T.LParen, T.Tilde, T.At, T.RParen
          , T.Dedent
        , T.Dedent ]

      <> ast
        [ namedU0Func "drawWidget"
              ( Sig0 PWrite [Param Imut TNat "width", Param Imut TNat "height"] $ Nothing )
              ExplicitRet $ Block0
              [ SVar
                $ Named "w" $ Var0 Imut Nothing $ e0Cons "Widget" $ Args Pure [e0Name "width", e0Name "height"]
              , SIf
                $ Iff
                  $ CondBlock
                    ( e0Select (e0Name "w") "exists" ) $ Block0
                    [ SExpr $ e0App (e0Select (e0Name "w") "draw") $ Args PWrite [] ]
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
      [ namedU0Func "quadratic"
        ( Sig0 Pure [Param Imut TFlt "a", Param Imut TFlt "b", Param Imut TFlt "c"]
          $ Just $ TFunc Pure [TFlt] $ TFlt
        ) ImplicitRet $ Block0
        [ SExpr
          $ e0Lambda
              ( Sig0 Pure [Param Imut TFlt "x"] $ Just TFlt ) ImplicitRet $ Block0
              [ SExpr
                $ e0BinOp Add
                  ( e0BinOp Mul (e0Name "a") $ e0BinOp Mul (e0Name "x") (e0Name "x") )
                  $ e0BinOp Add
                    ( e0BinOp Mul ( e0Name "b") $ e0Name "x" )
                    $ e0Name "c"
              ]
        ]
      ]

  , name "quadratic (implicit return types)"
    <> source
      "quadratic(Flt a, Flt b, Flt c) =>\n\
      \    (Flt x) =>\n\
      \        a*x*x + b*x + c"
    <> ast
      [ namedU0Func "quadratic"
        ( Sig0 Pure [Param Imut TFlt "a", Param Imut TFlt "b", Param Imut TFlt "c"] Nothing) ImplicitRet $ Block0
        [ SExpr
          $ e0Lambda
            ( Sig0 Pure [Param Imut TFlt "x"] Nothing) ImplicitRet $ Block0
            [ SExpr
              $ e0BinOp Add
                ( e0BinOp Mul (e0Name "a") $ e0BinOp Mul (e0Name "x") (e0Name "x") )
                $ e0BinOp Add
                  ( e0BinOp Mul (e0Name "b") $ e0Name "x" )
                  $ e0Name "c"
            ]
        ]
      ]

  , name "quadratic formula (single root)"
    <> source
      "singleRoot(Flt a, Flt b, Flt c) -> Flt =>\n\
      \    (-b + math.sqrt(b*b - 4*a*c)) / 2*a"
    <> ast
      [ namedU0Func "singleRoot"
        ( Sig0 Pure [Param Imut TFlt "a", Param Imut TFlt "b", Param Imut TFlt "c"] $ Just TFlt ) ImplicitRet $ Block0
        [ SExpr
          $ e0BinOp Div
            ( e0BinOp Add
              ( e0UnOp Neg (e0Name "b") )
              $ e0App
                (e0Select (e0Name "math") "sqrt" )
                $ Args
                  Pure
                  [ e0BinOp Sub
                    (e0BinOp Mul (e0Name "b") (e0Name "b"))
                    (e0BinOp Mul (e0ValInt 4) $ e0BinOp Mul (e0Name "a") (e0Name "c"))
                  ]
            )
            (e0BinOp Mul (e0ValInt 2) (e0Name "a"))
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
    <> typeErrors [failedToUnify TBln TInt]

  , source "Int a = true"
    <> typedAst [("a", UVar $ Var2 Imut TInt $ e2ValBln True)]
    <> typeErrors [failedToUnify TInt TBln]

  , source "$ a = b"
    <> typedAst [("a", UVar $ Var2 Imut (TError $ UnknownId "b")
                   $ Expr2 (TError $ UnknownId "b") $ EName "b")]
    <> typeErrors [UnknownId "b"]

  -- TODO: a proper error type and error handling for recursive definitions
  , source "$ a = a"
    <> typedAst [("a", UVar $ Var2 Imut (TError $ recursiveDefinition ["a"])
                   $ Expr2 (TError $ recursiveDefinition ["a"]) $ EName "a")]

    <> typeErrors [recursiveDefinition ["a"]]

  , source "$ a = b; $ b = a"
    <> let recErr = TError $ recursiveDefinition ["a", "b"]
       in typedAst [ ("a", UVar $ Var2 Imut recErr $ Expr2 recErr $ EName "b")
                   , ("b", UVar $ Var2 Imut recErr $ Expr2 recErr $ EName "a")]
    <> typeErrors [recursiveDefinition ["a", "b"]]


  , source "$ a = b; $ b = b"
    <> let recDef = recursiveDefinition ["b"]
           bType = TError recDef
       in typedAst
          [ ("a", UVar $ Var2 Imut (TError Propagated) $ Expr2 (TError Propagated) $ EName "b")
          , ("b", UVar $ Var2 Imut bType $ Expr2 bType $ EName "b")]

    <> typeErrors [recursiveDefinition ["b"]]


  , source "$ a = b; $ b = c; $ c = a"

    <> let cycleT = TError $ recursiveDefinition ["a", "b", "c"] in
       typedAst [ ("a", UVar $ Var2 Imut cycleT $ Expr2 cycleT $ EName "b")
                , ("b", UVar $ Var2 Imut cycleT $ Expr2 cycleT $ EName "c")
                , ("c", UVar $ Var2 Imut cycleT $ Expr2 cycleT $ EName "a")]

    <> typeErrors [recursiveDefinition ["a", "b", "c"]]


  , source "$ a = b; $ b = c; $ c = b"
    <> let cycle = recursiveDefinition ["b", "c"]
           cycleT = TError cycle in
       typedAst [ ("a", UVar $ Var2 Imut (TError Propagated) $ Expr2 (TError Propagated) $ EName "b")
                , ("b", UVar $ Var2 Imut cycleT $ Expr2 cycleT $ EName "c")
                , ("c", UVar $ Var2 Imut cycleT $ Expr2 cycleT $ EName "b")]
    <> typeErrors [recursiveDefinition ["b", "c"]]


  , source "$ a = 1; $ b = b + a"

    <> typedAst [ ("a", UVar $ Var2 Imut TInt $ e2ValInt 1)
                , ("b", UVar $ Var2 Imut (TError Propagated) $ Expr2 (TError Propagated)
                    $ EBinOp Add
                      (Expr2 (TError $ recursiveDefinition ["b"]) $ EName "b")
                      (Expr2 TInt $ EName "a"))]

    <> typeErrors [recursiveDefinition ["b"]]


  , source "$ a = true; $ a = false; $ b = a"
    <> typeErrors [CompetingDefinitions]

  , source "$ a = true; a() => true; $ b = a"
    <> typeErrors [CompetingDefinitions]

  , source "$ a = true; $ b = a"
    <> typedAst [ ("a", UVar $ Var2 Imut TBln $ e2ValBln True)
                , ("b", UVar $ Var2 Imut TBln $ e2Name TBln "a")]
    <> typeErrors []

  , source "$ a = b; $ b = true"
    <> typedAst [ ("a", UVar $ Var2 Imut TBln $ e2Name TBln "b")
                , ("b", UVar $ Var2 Imut TBln $ e2ValBln True)]
    <> typeErrors []

  , source "$ a = 5; Bln b = a"
    <> typeErrors [failedToUnify TBln TInt]

  , source "$ a = 5; $ b = a; $ c = b"
    <> typedAst [ ("a", UVar $ Var2 Imut TInt $ e2ValInt 5)
                , ("b", UVar $ Var2 Imut TInt $ e2Name TInt "a")
                , ("c", UVar $ Var2 Imut TInt $ e2Name TInt "b")]
    <> typeErrors []

  , source "$ a = 5; $ b = a; Bln c = b"
    <> typedAst [ ("a", UVar $ Var2 Imut TInt $ e2ValInt 5)
                , ("b", UVar $ Var2 Imut TInt $ e2Name TInt "a")
                , ("c", UVar $ Var2 Imut TBln $ e2Name TInt "b")]
    <> typeErrors [failedToUnify TBln TInt]

  , source "Bln a = b; $ b = c; $ c = 5"
    <> typedAst [ ("a", UVar $ Var2 Imut TBln $ e2Name TInt "b")
                , ("b", UVar $ Var2 Imut TInt $ e2Name TInt "c")
                , ("c", UVar $ Var2 Imut TInt $ e2ValInt 5)]
    <> typeErrors [failedToUnify TBln TInt]


  -- TypeCheck operator tests
  , source "$ a = 3 + 7"
    <> typedAst [ ("a", UVar $ Var2 Imut TInt $ e2BinOp TInt Add (e2ValInt 3) (e2ValInt 7))]
    <> typeErrors []

  , source "$ a = b + c; $ b = 3; $ c = 7"
    <> typedAst [ ("a", UVar $ Var2 Imut TInt $ e2BinOp TInt Add (e2Name TInt "b") (e2Name TInt "c"))
                , ("b", UVar $ Var2 Imut TInt $ e2ValInt 3)
                , ("c", UVar $ Var2 Imut TInt $ e2ValInt 7)]
    <> typeErrors []

  , source "$ a = 1 if true else 0"
    <> typedAst [("a", UVar $ Var2 Imut TInt $ e2If TInt (e2ValInt 1) (e2ValBln True) (e2ValInt 0))]
    <> typeErrors []

  , source "$ a = 1 if \"true\" else 0"
    <> typedAst [("a", UVar $ Var2 Imut TInt $ e2If TInt (e2ValInt 1) (e2ValStr "true") (e2ValInt 0))]
    <> typeErrors [failedToUnify TBln TStr]

  , source "$ a = 1 if true else \"zero\""
    <> typedAst [("a", UVar $ Var2 Imut TInt $ e2If TInt (e2ValInt 1) (e2ValBln True) (e2ValStr "zero"))]
    <> typeErrors [failedToUnify TInt TStr]


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
      [ ( "one", UFunc $ Func1
        ( Sig2 Pure [] TInt) $ Block1 [] (Just $ e2ValInt 1)
        )
      , ( "a", UVar $ Var2 Imut TInt $ e2App TInt (e2Name (TFunc Pure [] TInt) "one") $ Args Pure [])
      ]
    <> typeErrors []

  , name "one explicit, wrong return type"
    <> source
      "one() -> Int => \"one\""
    <> typeErrors [failedToUnify TInt TStr]

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
      [ ( "inc", UFunc $ Func1
          ( Sig2 Pure [Param Imut TInt "x"] TInt ) $ Block1
            [] (Just $ e2BinOp TInt Add (e2Name TInt "x") $ e2ValInt 1)
        )
      , ( "a", UVar $ Var2 Imut TInt $
          e2App TInt (e2Name (TFunc Pure [TInt] TInt) "inc") $ Args Pure [e2ValInt 1] )
      ]
    <> typeErrors []

  , name "inc explicit, nested calls"
    <> source
      "inc(Int x) -> Int => x + 1\n\
      \$ a = inc(inc(1))"
    <> let incType = TFunc Pure [TInt] TInt in
      typedAst
      [ ( "inc", UFunc $ Func1
          ( Sig2 Pure [Param Imut TInt "x"] TInt ) $ Block1 []
            (Just $ e2BinOp TInt Add (e2Name TInt "x") $ e2ValInt 1)
        )
      , ( "a", UVar $ Var2 Imut TInt $
          e2App TInt (e2Name incType "inc") $ Args Pure
            [e2App TInt (e2Name incType "inc") $ Args Pure [e2ValInt 1]]
        )
      ]
    <> typeErrors []

  , name "inc explicit, wrong return type"
    <> source
      "inc(Int x) -> Int => \"one\""
    <> typeErrors [failedToUnify TInt TStr]

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
      [ ("inc", UFunc $ Func1
          ( Sig2 Pure [Param Imut TInt "x"] TInt ) $ Block1 []
          ( Just $ e2BinOp TInt Add (e2Name TInt "x") $ e2ValInt 1 )
        )
      , ("a", UVar $ Var2 Imut TInt $ e2App TInt (e2Name (TFunc Pure [TInt] TInt) "inc") $ Args Pure [e2ValInt 1])
      ]
    <> typeErrors []

  , name "inc implicit, wrong arg type"
    <> source
      "inc(Int x) => x + 1\n\
      \$ a = inc(\"one\")"
    <> typedAst
      [ ("inc", UFunc $ Func1
          ( Sig2 Pure [Param Imut TInt "x"] TInt ) $ Block1 []
          ( Just $ e2BinOp TInt Add (e2Name TInt "x") $ e2ValInt 1 )
        )
      , ("a", UVar $ Var2 Imut TInt
          $ e2App TInt (e2Name (TFunc Pure [TInt] TInt) "inc") $ Args Pure [e2ValStr "one"])
      ]
    <> typeErrors [failedToUnify TInt TStr]

  , name "inc implicit, local var"
    <> source
      "inc(Int x) =>\n\
      \    $ one = 1\n\
      \    x + one\n\
      \\n\
      \$ a = inc(1)"
    <> typedAst
      [ ( "inc", UFunc $ Func1
          ( Sig2 Pure [Param Imut TInt "x"] TInt ) $ Block1
          [ SVar $ Named "one" $ Var2 Imut TInt (e2ValInt 1) ]
          ( Just$ e2BinOp TInt Add (e2Name TInt "x") (e2Name TInt "one") )
        )
      , ( "a", UVar $ Var2 Imut TInt
          $ e2App TInt (e2Name (TFunc Pure [TInt] TInt) "inc") $ Args Pure [e2ValInt 1])
      ]
    <> typeErrors []


  -- Evaluation tests

  , name "negative value"
    <> source
      "main() => -1 "
    <> returnVal (-1)

  , name "double 8"
    <> source
      "double(Int i) => 2 * i\n\
      \main() => double(8)"
    <> returnVal 16

  , name "inc 7"
    <> source
      "inc(Int i) => i + 1\n\
      \main() => inc(7)"
    <> returnVal 8

  , name "square 6"
    <> source
      "sqr(Int i) => i * i\n\
      \main() => sqr(6)"
    <> returnVal 36

  , name "sum to 5"
    <> source
      "sumTo(Int i) => i * (i + 1) / 2\n\
      \main() => sumTo(5)"
    <> returnVal 15

  , name "factorial 4"
    <> source
      "fact(Int i) -> Int => 1 if i <= 1 else i * fact(i - 1)\n\
      \main() => fact(5)"
    <> returnVal 120

  , name "fibonacci 7"
    <> source
      "fib(Int i) => 1 if i <= 1 else fib(i - 1) + fib(i - 2)\n\
      \main() => fib(7)"
    <> returnVal 21

  , name "gcd 18 24"
    <> source
      "gcd(Int a, Int b) => a if b == 0 else gcd(b, a % b)\n\
      \main() => gcd(18, 24)"
    <> returnVal 6

  -- Modulus operator tests
  , source "main() => 0 % 2"
    <> returnVal 0

  , source "main() => 5 % 5"
    <> returnVal 0

  , source "main() => 23 % 7"
    <> returnVal 2

  , source "main() => -3 % 7"
    <> returnVal 4

  -- Comparison operator tests

    -- Greater
  , source "main() => 2 > 1"
    <> returnVal 1

  , source "main() => 1 > 2"
    <> returnVal 0

  , source "main() => 1 > -2"
    <> returnVal 1

  , source "main() => -2 > 1"
    <> returnVal 0

    -- Lesser
  , source "main() => 1 < 2"
    <> returnVal 1

  , source "main() => 2 < 1"
    <> returnVal 0

  , source "main() => -2 < 1"
    <> returnVal 1

  , source "main() => 1 < -2"
    <> returnVal 0

    -- Greater-equal
  , source "main() => 2 >= 1"
    <> returnVal 1

  , source "main() => 1 >= 2"
    <> returnVal 0

  , source "main() => 1 >= -2"
    <> returnVal 1

  , source "main() => -2 >= 1"
    <> returnVal 0

  , source "main() => 1 >= 1"
    <> returnVal 1

  , source "main() => -2 >= -2"
    <> returnVal 1

    -- Lesser
  , source "main() => 1 <= 2"
    <> returnVal 1

  , source "main() => 2 <= 1"
    <> returnVal 0

  , source "main() => -2 <= 1"
    <> returnVal 1

  , source "main() => 1 <= -2"
    <> returnVal 0

  , source "main() => 1 <= 1"
    <> returnVal 1

  , source "main() => -2 <= -2"
    <> returnVal 1

    -- Equal
  , source "main() => 0 == 0"
    <> returnVal 1

  , source "main() => 3 == 4"
    <> returnVal 0

  , source "main() => 0 != 0"
    <> returnVal 0

  , source "main() => 3 != 4"
    <> returnVal 1

  -- If expressions
  , source "main() => 3 if 1 < 2 else 7"
    <> returnVal 3

  , source "main() => 6 if false else -1"
    <> returnVal (-1)

 ]

