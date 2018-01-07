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
    [ Named "three" $ UVar $ Var Imt Nothing $ EApp
      $ App (EName "+") $ Args Pure [EVal $ VInt 1, EVal $ VInt 2] ]

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
          (Cond $ EApp $ App (EName "<=") $ Args Pure [EName "n", EVal $ VInt 0])
          (EVal $ VInt 1)
          (EApp $ App (EName "*") $ Args Pure
           [ (EName "n")
           , EApp $ App (EName "factorial") $ Args Pure
             [EApp $ App (EName "-") $ Args Pure [EName "n", EVal $ VInt 1]]
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
        $ EIf (Cond $ ESelect $ Select (EName "w") "isRaining") (EName "rainCoat")
        $ EIf (Cond $ ESelect $ Select (EName "w") "isCold") (EName "coat")
        $ EIf (Cond $ ESelect $ Select (EName "w") "isSunny") (EName "tShirt")
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
        $ EIf (Cond $ ESelect $ Select (EName "w") "isRaining") (EName "rainCoat")
        $ EIf (Cond $ ESelect $ Select (EName "w") "isCold") (EName "coat")
        $ EIf (Cond $ ESelect $ Select (EName "w") "isSunny") (EName "tShirt")
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
        $ Named "w" $ Var Imt Nothing $ EApp $ App (ECons "Widget") $ Args Pure
          [EName "width", EName "height"]
      , SIf
        $ If
          $ CondBlock
            ( ESelect $ Select (EName "w") "exists" )
            [ SExpr $ EApp $ App (ESelect $ Select (EName "w") "draw") $ Args PWrite [] ]
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
            $ EApp $ App (EName "+") $ Args Pure
              [ EApp $ App (EName "*") $ Args Pure
                [ EName "a"
                , EApp $ App (EName "*") $ Args Pure [EName "x", EName "x"]
                ]
              , EApp $ App (EName "+") $ Args Pure
                [ EApp $ App (EName "*") $ Args Pure [EName "b", EName "x"]
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
            $ EApp $ App (EName "+") $ Args Pure
              [ EApp $ App (EName "*") $ Args Pure
                [ EName "a"
                , EApp $ App (EName "*") $ Args Pure [EName "x", EName "x"]
                ]
              , EApp $ App (EName "+") $ Args Pure
                [ EApp $ App (EName "*") $ Args Pure [EName "b", EName "x"]
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
      [ SExpr $ EApp $ App (EName "/") $ Args Pure
        [ EApp $ App (EName "+") $ Args Pure
          [ EApp $ App (EName "-") $ Args Pure [EName "b"]
          , EApp $ App (ESelect $ Select (EName "math") "sqrt") $ Args Pure
            [ EApp $ App (EName "-") $ Args Pure
              [ EApp $ App (EName "*") $ Args Pure [EName "b", EName "b"]
              , EApp $ App (EName "*") $ Args Pure
                [ EVal $ VInt 4
                , EApp $ App (EName "*") $ Args Pure [EName "a", EName "c"]
                ]
              ]
            ]
          ]
        , EApp $ App (EName "*") $ Args Pure [EVal $ VInt 2, EName "a"]
        ]
      ]
    ]

  , test "foo() => foo()"
    [ Named "foo" $ UFunc $ Func
      (Sig Pure [] Nothing)
      ImplicitRet
      [SExpr $ EApp $ App (EName "foo") $ Args Pure []]
    ]

  , test [s|
foo() =>
    foo()
|]
    [ Named "foo" $ UFunc $ Func
      (Sig Pure [] Nothing)
      ImplicitRet
      [SExpr $ EApp $ App (EName "foo") $ Args Pure []]
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
      [ SVar $ Named "arr" $ Var Mut Nothing $ EApp $ App (ECons "Array") $ Args Pure
        [EVal $ VInt 2, EVal $ VInt 0]
      , SAssign (LApp $ App (EName "arr") $ Args Pure [EVal $ VInt 0]) (EVal $ VInt 1)
      , SAssign (LApp $ App (EName "arr") $ Args Pure [EVal $ VInt 1]) (EVal $ VInt 2)
      , SExpr $ EApp $ App (EName "arr") $ Args Pure [EVal $ VInt 0]
      ]
    ]
  ]

