{-# language QuasiQuotes #-}
module Test.Parser(tests) where

import Control.Monad(unless)
import Data.String.QQ

import Test.Tasty
import Test.Tasty.HUnit

import Ast.A0Parse
import Lexer
import Parser
import Util.PrettyShow

test :: String -> Ast -> TestTree
test src = namedTest src src

namedTest :: String -> String -> Ast -> TestTree
namedTest name src ast =
  let ast' = parse $ scanTokens src
  in testCase name $ unless (ast' == ast) $ assertFailure $
    "expected ast:\n" ++ prettyShow ast ++ "\nbut got:\n" ++ prettyShow ast'

exprTest :: String -> Expr -> TestTree
exprTest src = namedExprTest src src

namedExprTest :: String -> String -> Expr -> TestTree
namedExprTest name src expr =
  let expr' = parseExpr $ scanTokens src
  in testCase name $ unless (expr' == expr) $ assertFailure $
     "expected expr:\n" ++ prettyShow expr ++ "\nbut got:\n" ++ prettyShow expr'

tests :: TestTree
tests = testGroup "parser"

  [ namedTest "empty str" "" []

  , namedTest "def pi"
    "$ pi = 3.14159265"
    [Named "pi" $ UVar $ Var Imt Nothing $ EVal $ VFlt 3.14159265]

  , namedTest "op expr"
    "$ three = 1 + 2"
    [ Named "three" $ UVar $ Var Imt Nothing
      $ EApp (EName "+") Pure [EVal $ VInt 1, EVal $ VInt 2] ]

  , namedTest "if expr"
    [s|$ msg = if true then "it works!" else "or not :("|]
    [ Named "msg" $ UVar $ Var Imt Nothing
    $ EIf (EVal $ VBln True)
      [EVal $ VStr "it works!"]
      [EVal $ VStr "or not :("]
    ]

  , namedTest "negate inline"
    "negate(Bln b) -> Bln => if b then false else true"
    [ Named "negate" $ UFunc $ Func (Sig Pure [Param Imt TBln "b"] $ Just TBln)
      [ EIf (EName "b")
        [EVal $ VBln False]
        [EVal $ VBln True]
      ]
    ]

  , namedTest "negate block"
    [s|
negate(Bln b) -> Bln =>
    if b then false else true
    |]
    [ Named "negate" $ UFunc $ Func (Sig Pure [Param Imt TBln "b"] $ Just TBln)
      [EIf (EName "b") [EVal $ VBln False] [EVal $ VBln True] ]
    ]

  , namedTest "factorial"
    [s|
factorial(Int n) -> Int =>
    if n <= 0 then 1 else n * factorial(n - 1)
    |]
    [ Named "factorial" $ UFunc $ Func ( Sig Pure [Param Imt TInt "n"] $ Just TInt)
      [ EIf
        (EApp (EName "<=") Pure [EName "n", EVal $ VInt 0])
        [EVal $ VInt 1]
        [EApp (EName "*") Pure
          [ (EName "n")
          , EApp (EName "factorial") Pure
            [EApp (EName "-") Pure [EName "n", EVal $ VInt 1]]
          ]
        ]
      ]
    ]

  , namedTest "cascading if exprs"
    [s|
clothing(Weather w) -> Clothing =>
    if w.isRaining then rainCoat else if w.isClold then coat else if w.isSunny then tShirt else jacket
    |]
    [ Named "clothing" $ UFunc $ Func
      (Sig Pure [Param Imt (TUser "Weather") "w"] $ Just $ TUser "Clothing")
      [EIf (ESelect (EName "w") "isRaining")
        [EName "rainCoat"]
        [EIf (ESelect (EName "w") "isCold")
          [EName "coat"]
          [EIf (ESelect (EName "w") "isSunny")
            [EName "tShirt"]
            [EName "jacket"]
          ]
        ]
      ]
    ]

  , namedTest "cascading if exprs multiline"

--     [s|
-- clothing(Weather w) -> Clothing =>
--     if w.isRaining then rainCoat else
--     if w.isCold then coat else
--     if w.isSunny then tShirt else
--     jacket
--     |]

    [s|
clothing(Weather w) -> Clothing =>
    if w.isRaining then rainCoat
    else if w.isCold then coat
    else if w.isSunny then tShirt
    else jacket
    |]
    [ Named "clothing" $ UFunc $ Func
      (Sig Pure [Param Imt (TUser "Weather") "w"] $ Just $ TUser "Clothing")
      [EIf (ESelect (EName "w") "isRaining")
        [EName "rainCoat"]
        [EIf (ESelect (EName "w") "isCold")
          [EName "coat"]
          [EIf (ESelect (EName "w") "isSunny")
            [EName "tShirt"]
            [EName "jacket"]
          ]
        ]
      ]
    ]
  , namedTest "draw widget"
    [s|
drawWidget(~@, Nat width, Nat height) =>
    $ w = Widget(width, height)
    if w.exists then
        w.draw(~@)
    |]
    [ Named "drawWidget" $ UFunc $ Func
      ( Sig PWrite [Param Imt TNat "width", Param Imt TNat "height"] $ Nothing )
      [ EVar $ Named "w" $ Var Imt Nothing $ EApp (EName "Widget") Pure
          [EName "width", EName "height"]
      , EIf (ESelect (EName "w") "exists")
        [EApp (ESelect (EName "w") "draw") PWrite []]
        []
      ]
    ]

  , namedTest "quadratic (explicit return types)"
    [s|
quadratic(Flt a, Flt b, Flt c) -> Flt -> Flt =>
    (Flt x) -> Flt =>
        a*x*x + b*x + c
    |]
    [ Named "quadratic" $ UFunc $ Func
      ( Sig Pure [Param Imt TFlt "a", Param Imt TFlt "b", Param Imt TFlt "c"]
        $ Just $ TFunc Pure [TFlt] $ TFlt
      )
      [ ELambda $ Func
          ( Sig Pure [Param Imt TFlt "x"] $ Just TFlt )
          [ EApp (EName "+") Pure
            [ EApp (EName "*") Pure
              [ EName "a"
              , EApp (EName "*") Pure [EName "x", EName "x"]
              ]
            , EApp (EName "+") Pure
              [ EApp (EName "*") Pure [EName "b", EName "x"]
              , EName "c"
              ]
            ]
          ]
      ]
    ]

  , namedTest "quadratic (implicit return types)"
    [s|
quadratic(Flt a, Flt b, Flt c) =>
    (Flt x) =>
        a*x*x + b*x + c
    |]
    [ Named "quadratic" $ UFunc $ Func
      ( Sig Pure [Param Imt TFlt "a", Param Imt TFlt "b", Param Imt TFlt "c"] Nothing)
      [ ELambda $ Func
        ( Sig Pure [Param Imt TFlt "x"] Nothing)
        [ EApp (EName "+") Pure
          [ EApp (EName "*") Pure
            [ EName "a"
            , EApp (EName "*") Pure [EName "x", EName "x"]
            ]
          , EApp (EName "+") Pure
            [ EApp (EName "*") Pure [EName "b", EName "x"]
            , EName "c"
            ]
          ]
          ]
      ]
    ]

  , namedTest "quadratic formula (single root)"
    [s|
singleRoot(Flt a, Flt b, Flt c) -> Flt =>
    (-b + math.sqrt(b*b - 4*a*c)) / 2*a
      |]
    [ Named "singleRoot" $ UFunc $ Func
      ( Sig Pure [Param Imt TFlt "a", Param Imt TFlt "b", Param Imt TFlt "c"] $ Just TFlt )
      [ EApp (EName "/") Pure
        [ EApp (EName "+") Pure
          [ EApp (EName "-") Pure [EName "b"]
          , EApp (ESelect (EName "math") "sqrt") Pure
            [ EApp (EName "-") Pure
              [ EApp (EName "*") Pure [EName "b", EName "b"]
              , EApp (EName "*") Pure
                [ EVal $ VInt 4
                , EApp (EName "*") Pure [EName "a", EName "c"]
                ]
              ]
            ]
          ]
        , EApp (EName "*") Pure [EVal $ VInt 2, EName "a"]
        ]
      ]
    ]

  , test "foo() => foo()"
    [ Named "foo" $ UFunc $ Func
      (Sig Pure [] Nothing)
      [EApp (EName "foo") Pure []]
    ]

  , test [s|
foo() =>
    foo()
|]
    [ Named "foo" $ UFunc $ Func
      (Sig Pure [] Nothing)
      [EApp (EName "foo") Pure []]
    ]

  , namedTest "array" [s|
f() =>
    ~$ arr = Array(2, 0)
    arr(0) = 1
    arr(1) = 2
    arr(0)
|]
    [ Named "f" $ UFunc $ Func
      (Sig Pure [] Nothing)
      [ EVar $ Named "arr" $ Var Mut Nothing $ EApp (EName "Array") Pure
        [EVal $ VInt 2, EVal $ VInt 0]
      , EAssign (EApp (EName "arr") Pure [EVal $ VInt 0]) (EVal $ VInt 1)
      , EAssign (EApp (EName "arr") Pure [EVal $ VInt 1]) (EVal $ VInt 2)
      , EApp (EName "arr") Pure [EVal $ VInt 0]
      ]
    ]

  , testGroup "precedence"
    [ exprTest "1 == 2 or \"red\" == \"blue\"" $
      EApp (EName "or") Pure
      [ EApp (EName "==") Pure
        [ EVal $ VInt 1
        , EVal $ VInt 2
        ]
      , EApp (EName "==") Pure
        [ EVal $ VStr "red"
        , EVal $ VStr "blue"
        ]
      ]

    , exprTest "1 * 2 + 3 * 4" $
      EApp (EName "+") Pure
      [ EApp (EName "*") Pure
        [ EVal $ VInt 1
        , EVal $ VInt 2
        ]
      , EApp (EName "*") Pure
        [ EVal $ VInt 3
        , EVal $ VInt 4
        ]
      ]

    , exprTest "1 + 2 == 3 + 4" $
      EApp (EName "==") Pure
      [ EApp (EName "+") Pure
        [ EVal $ VInt 1
        , EVal $ VInt 2
        ]
      , EApp (EName "+") Pure
        [ EVal $ VInt 3
        , EVal $ VInt 4
        ]
      ]

    , exprTest "a and b or c and d" $
      EApp (EName "or") Pure
      [ EApp (EName "and") Pure
        [ EName "a"
        , EName "b"]
      , EApp (EName "and") Pure
        [ EName "c"
        , EName "d"
        ]
      ]
    ]

  , testGroup "if"
    [ exprTest "if a then b" $
      EIf (EName "a") [EName "b"] []

    , exprTest "if a then b else c" $
      EIf (EName "a") [EName "b"] [EName "c"]

    , exprTest "if a then b else if c then d" $
      EIf (EName "a")
        [EName "b"]
        [EIf (EName "c") [EName "d"] [] ]

    , exprTest "if a then b else if c then d else e" $
      EIf (EName "a")
        [EName "b"]
        [EIf (EName "c") [EName "d"] [EName "e"] ]

    , namedExprTest "if-multiline" [s|
if a then b else
if c then d else
e
|] $
      EIf (EName "a")
        [EName "b"]
        [EIf (EName "c") [EName "d"] [EName "e"] ]

    , namedExprTest "single-if-multiline" [s|
if a then
    b
else
    c
|] $
      EIf (EName "a")
        [EName "b"]
        [EName "c"]
    ]


  , testGroup "user-types"
    [ namedTest "empty" "data Empty"
      [Named "Empty" $ UData $ Data []]

    , namedTest "vector" [s|
data Vector
    Flt x
    Flt y
    Flt z
|]
    [ Named "Vector" $ UData $ Data
      [ Named "x" $ MVar Pub TFlt
      , Named "y" $ MVar Pub TFlt
      , Named "z" $ MVar Pub TFlt
      ]
    ]

    , namedTest "colour" [s|
data Colour
    Int r
    Int g
    Int b
    Int a
|]
      [ Named "Colour" $ UData $ Data
        [ Named "r" $ MVar Pub TInt
        , Named "g" $ MVar Pub TInt
        , Named "b" $ MVar Pub TInt
        , Named "a" $ MVar Pub TInt
        ]
      ]
    ]
  ]

