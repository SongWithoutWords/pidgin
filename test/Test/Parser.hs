{-# language QuasiQuotes #-}
module Test.Parser(tests) where

import Data.String.QQ

import Test.Tasty
import qualified Test.Tasty.HUnit as H

import Ast.A0Parse
import Lexer
import Parser

test :: String -> String -> Ast -> TestTree
test name input expected = H.testCase name $ (parse $ scanTokens input) H.@?= expected

tests :: TestTree
tests = testGroup "parser"

  [ test "empty str" "" []

  , test "def pi"
    "$ pi = 3.14159265"
    [Named "pi" $ UVar $ Var Imt Nothing $ EVal $ VFlt 3.14159265]

  , test "op expr"
    "$ three = 1 + 2"
    [ Named "three" $ UVar $ Var Imt Nothing $ EBinOp Add (EVal $ VInt 1) (EVal $ VInt 2) ]

  , test "if expr"
    [s|$ msg = "it works!" if true else "or not :("|]
    [ Named "msg" $ UVar $ Var Imt Nothing
    $ EIf (Cond $ EVal $ VBln True) (EVal $ VStr "it works!") (EVal $ VStr "or not :(") ]

  , test "negate inline"
    "negate(Bln b) -> Bln => false if b else true"
    [ Named "negate" $ UFunc $ Func (Sig Pure [Param Imt TBln "b"] $ Just TBln) ImplicitRet
      [ SExpr $ EIf (Cond $ EName "b") (EVal $ VBln False) (EVal $ VBln True) ]
    ]

  , test "negate block"
    [s|
negate(Bln b) -> Bln =>
    false if b else true
    |]
    [ Named "negate" $ UFunc $ Func (Sig Pure [Param Imt TBln "b"] $ Just TBln) ImplicitRet
      [ SExpr $ EIf (Cond $ EName "b") (EVal $ VBln False) (EVal $ VBln True) ]
    ]

  , test "factorial"
    [s|
factorial(Int n) -> Int =>
    1 if n <= 0 else n * factorial(n-1)
    |]
    [ Named "factorial" $ UFunc $ Func ( Sig Pure [Param Imt TInt "n"] $ Just TInt) ImplicitRet
      [ SExpr
        $ EIf
          (Cond $ EBinOp (Cmp LesserEq) (EName "n") (EVal $ VInt 0))
          (EVal $ VInt 1)
          (EBinOp Mul
              (EName "n")
              $ EApp $ App (EName "factorial") $ Args Pure [EBinOp Sub (EName "n") (EVal $ VInt 1)]
          )
      ]
    ]

  , test "cascading if exprs"
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

  , test "cascading if exprs multiline"
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

  , test "draw widget"
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
        $ Named "w" $ Var Imt Nothing $ ECons "Widget" $ Args Pure [EName "width", EName "height"]
      , SIf
        $ If
          $ CondBlock
            ( ESelect $ Select (EName "w") "exists" )
            [ SExpr $ EApp $ App (ESelect $ Select (EName "w") "draw") $ Args PWrite [] ]
      ]
    ]

  , test "quadratic (explicit return types)"
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
              $ EBinOp Add
                ( EBinOp Mul (EName "a") $ EBinOp Mul (EName "x") (EName "x") )
                $ EBinOp Add
                  ( EBinOp Mul (EName "b") $ EName "x" )
                  $ EName "c"
            ]
      ]
    ]

  , test "quadratic (implicit return types)"
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
            $ EBinOp Add
              ( EBinOp Mul (EName "a") $ EBinOp Mul (EName "x") (EName "x") )
              $ EBinOp Add
                ( EBinOp Mul (EName "b") $ EName "x" )
                $ EName "c"
          ]
      ]
    ]

  , test "quadratic formula (single root)"
    [s|
singleRoot(Flt a, Flt b, Flt c) -> Flt =>
    (-b + math.sqrt(b*b - 4*a*c)) / 2*a
      |]
    [ Named "singleRoot" $ UFunc $ Func
      ( Sig Pure [Param Imt TFlt "a", Param Imt TFlt "b", Param Imt TFlt "c"] $ Just TFlt )
      ImplicitRet
      [ SExpr
        $ EBinOp Div
          ( EBinOp Add
            ( EUnOp Neg (EName "b") )
            $ EApp $ App
              (ESelect $ Select (EName "math") "sqrt" )
              $ Args
                Pure
                [ EBinOp Sub
                  (EBinOp Mul (EName "b") (EName "b"))
                  (EBinOp Mul (EVal $ VInt 4) $ EBinOp Mul (EName "a") (EName "c"))
                ]
          )
          (EBinOp Mul (EVal $ VInt 2) (EName "a"))
        ]
    ]
  ]

