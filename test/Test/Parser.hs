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
    [s|$ msg = "it works!" if true else "or not :("|]
    [ Named "msg" $ UVar $ Var Imt Nothing
    $ EIf (Cond $ EVal $ VBln True) (EVal $ VStr "it works!") (EVal $ VStr "or not :(") ]

  , namedTest "negate inline"
    "negate(Bln b) -> Bln => false if b else true"
    [ Named "negate" $ UFunc $ Func (Sig Pure [Param Imt TBln "b"] $ Just TBln) ImplicitRet
      [ SExpr $ EIf (Cond $ EName "b") (EVal $ VBln False) (EVal $ VBln True) ]
    ]

  , namedTest "negate block"
    [s|
negate(Bln b) -> Bln =>
    false if b else true
    |]
    [ Named "negate" $ UFunc $ Func (Sig Pure [Param Imt TBln "b"] $ Just TBln) ImplicitRet
      [ SExpr $ EIf (Cond $ EName "b") (EVal $ VBln False) (EVal $ VBln True) ]
    ]

  , namedTest "factorial"
    [s|
factorial(Int n) -> Int =>
    1 if n <= 0 else n * factorial(n - 1)
    |]
    [ Named "factorial" $ UFunc $ Func ( Sig Pure [Param Imt TInt "n"] $ Just TInt) ImplicitRet
      [ SExpr
        $ EIf
          (Cond $ EApp (EName "<=") Pure [EName "n", EVal $ VInt 0])
          (EVal $ VInt 1)
          (EApp (EName "*") Pure
           [ (EName "n")
           , EApp (EName "factorial") Pure
             [EApp (EName "-") Pure [EName "n", EVal $ VInt 1]]
           ]
          )
      ]
    ]

  , namedTest "cascading if exprs"
    [s|
clothing(Weather w) -> Clothing =>
    rainCoat if w.isRaining else coat if w.isCold else tShirt if w.isSunny else jacket
    |]
    [ Named "clothing" $ UFunc $ Func
      ( Sig Pure [Param Imt (TUser "Weather") "w"] $ Just $ TUser "Clothing" ) ImplicitRet
      [ SExpr
        $ EIf (Cond $ ESelect (EName "w") "isRaining") (EName "rainCoat")
        $ EIf (Cond $ ESelect (EName "w") "isCold") (EName "coat")
        $ EIf (Cond $ ESelect (EName "w") "isSunny") (EName "tShirt")
        $ EName "jacket"
      ]
    ]

  , namedTest "cascading if exprs multiline"
    [s|
clothing(Weather w) -> Clothing =>
    rainCoat if w.isRaining else
    coat if w.isCold else
    tShirt if w.isSunny else
    jacket
    |]
    [ Named "clothing" $ UFunc $ Func
      ( Sig Pure [Param Imt (TUser "Weather") "w"] $ Just $ TUser "Clothing" ) ImplicitRet
      [ SExpr
        $ EIf (Cond $ ESelect (EName "w") "isRaining") (EName "rainCoat")
        $ EIf (Cond $ ESelect (EName "w") "isCold") (EName "coat")
        $ EIf (Cond $ ESelect (EName "w") "isSunny") (EName "tShirt")
        $ EName "jacket"
      ]
    ]

  , namedTest "draw widget"
    [s|
drawWidget(~@, Nat width, Nat height):
    $ w = Widget(width, height)
    if w.exists:
        w.draw(~@)
    |]
    [ Named "drawWidget" $ UFunc $ Func
      ( Sig PWrite [Param Imt TNat "width", Param Imt TNat "height"] $ Nothing )
      ExplicitRet
      [ SVar
        $ Named "w" $ Var Imt Nothing $ EApp (EName "Widget") Pure
          [EName "width", EName "height"]
      , SIf
        $ If
          $ CondBlock
            ( ESelect (EName "w") "exists" )
            [ SExpr $ EApp (ESelect (EName "w") "draw") PWrite [] ]
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
      ) ImplicitRet
      [ SExpr
        $ ELambda $ Func
          ( Sig Pure [Param Imt TFlt "x"] $ Just TFlt ) ImplicitRet
          [ SExpr
            $ EApp (EName "+") Pure
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
      ImplicitRet
      [ SExpr
        $ ELambda $ Func
          ( Sig Pure [Param Imt TFlt "x"] Nothing) ImplicitRet
          [ SExpr
            $ EApp (EName "+") Pure
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
      ImplicitRet
      [ SExpr $ EApp (EName "/") Pure
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
      ImplicitRet
      [SExpr $ EApp (EName "foo") Pure []]
    ]

  , test [s|
foo() =>
    foo()
|]
    [ Named "foo" $ UFunc $ Func
      (Sig Pure [] Nothing)
      ImplicitRet
      [SExpr $ EApp (EName "foo") Pure []]
    ]

  , namedTest "array" [s|
f() =>
    ~$ arr = Array(2, 0)
    arr(0) = 1
    arr(1) = 2
    arr(0)
|]
    [ Named "f" $ UFunc $ Func
      (Sig Pure [] Nothing) ImplicitRet
      [ SVar $ Named "arr" $ Var Mut Nothing $ EApp (EName "Array") Pure
        [EVal $ VInt 2, EVal $ VInt 0]
      , SAssign (EApp (EName "arr") Pure [EVal $ VInt 0]) (EVal $ VInt 1)
      , SAssign (EApp (EName "arr") Pure [EVal $ VInt 1]) (EVal $ VInt 2)
      , SExpr $ EApp (EName "arr") Pure [EVal $ VInt 0]
      ]
    ]
  ]

