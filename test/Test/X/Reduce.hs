module Test.X.Reduce(tests) where

import Data.Set as S
-- import Data.Map as M

import Test.Tasty
import Test.Tasty.HUnit

import X.Ast
import X.Error
import X.TypeCheck.Reduce

import Util.MultiMap

reduceExprTest :: String -> Expr -> Expr -> [Error] -> TestTree
reduceExprTest name input expr errorList =
  let
    errs = S.fromList errorList
    (expr', errs') = runReduce $ reduceExpr input
  in testGroup name
    [ testCase "expr" $ expr' @?= expr
    , testCase "errors" $ errs' @?= errs]

tests :: TestTree
tests = testGroup "reduce"
  [ (let
      unitsA = multiFromList
        [ ("b", UNamespace unitsB)
        , ("c", UData $ multiFromList [])
        ]
      unitsB = multiFromList
        [ ("d", UNamespace $ multiFromList [])
        , ("e", UData $ multiFromList [])
        ]
      input =
        Expr TError $ ESelect
          (Expr TError $ EName "a" [KNamespace unitsA])
          "b" []
      output = Expr TError $ EName "a.b" [KNamespace unitsB]

      in reduceExprTest "select-namespace" input output [])

  , (let
      unitsA = multiFromList
        [ ("b", UNamespace unitsB)
        , ("x", UData $ multiFromList [])
        ]
      unitsB = multiFromList
        [ ("c", UNamespace unitsC)
        , ("y", UData $ multiFromList [])
        ]
      unitsC = multiFromList
        [ ("d", UNamespace $ multiFromList [])
        , ("z", UData $ multiFromList [])
        ]
      input =
        Expr TError $ ESelect
          (Expr TError $ ESelect
            (Expr TError $ EName "a" [KNamespace unitsA])
            "b" []
          )
        "c" []
      output = Expr TError $ EName "a.b.c" [KNamespace unitsC]

     in reduceExprTest "select-namespace-nested" input output [])

  , (let
      members = multiFromList
        [ ("x", MVar Pub TFlt)
        , ("y", MVar Pub TFlt)
        ]
      vector = TData "Vector" members
      expr' = ESelect (Expr vector $ EName "x" [KExpr $ Expr vector EBinding]) "y" []
      input = Expr TError expr'
      output = Expr TFlt expr'
    in reduceExprTest "select-member" input output [])

  , testGroup "app"
    [ (let
      tadd = [TInt, TInt] ~> TInt
      expr' = EApp Pure (Expr tadd $ EName "+" [KExpr $ Expr tadd EBinding])
        [ Expr TInt $ EName "a" [KExpr $ Expr TInt EBinding]
        , Expr TInt $ EName "b" [KExpr $ Expr TInt EBinding]
        ]
      input = Expr TError expr'
      output = Expr TInt expr'
      in reduceExprTest "simple-app" input output [])

    , (let
      tadd = [TInt, TInt] ~> TInt
      expr' = EApp PWrite (Expr tadd $ EName "+" [KExpr $ Expr tadd EBinding])
        [ Expr TInt $ EName "a" [KExpr $ Expr TInt EBinding]
        , Expr TInt $ EName "b" [KExpr $ Expr TInt EBinding]
        ]
      input = Expr TError expr'
      output = Expr TInt expr'
      in reduceExprTest "wrong-purity" input output [WrongPurity Pure PWrite])

    , (let
        tadd = [TInt, TInt] ~> TInt
        expr' = EApp Pure (Expr tadd $ EName "+" [KExpr $ Expr tadd EBinding])
          [ Expr TInt $ EName "a" [KExpr $ Expr TInt EBinding]
          , Expr TInt $ EName "b" [KExpr $ Expr TInt EBinding]
          , Expr TInt $ EName "c" [KExpr $ Expr TInt EBinding]
          ]
        input = Expr TError expr'
        output = Expr TInt expr'
        in reduceExprTest "wrong-num-args" input output [WrongNumArgs 2 3])

    , (let
        tadd = [TInt, TInt] ~> TInt
        expr' = EApp Pure (Expr tadd $ EName "+" [KExpr $ Expr tadd EBinding])
          [ Expr TBln $ EName "a" [KExpr $ Expr TBln EBinding]
          , Expr TInt $ EName "b" [KExpr $ Expr TInt EBinding]
          ]
        input = Expr TError expr'
        output = Expr TInt expr'
        in reduceExprTest "first-arg-wrong-type" input output [WrongType TInt TBln])

    , (let
        tadd = [TInt, TInt] ~> TInt
        expr' = EApp Pure (Expr tadd $ EName "+" [KExpr $ Expr tadd EBinding])
          [ Expr TInt $ EName "a" [KExpr $ Expr TInt EBinding]
          , Expr TStr $ EName "b" [KExpr $ Expr TStr EBinding]
          ]
        input = Expr TError expr'
        output = Expr TInt expr'
        in reduceExprTest "second-arg-wrong-type" input output [WrongType TInt TStr])

    , (let
        tadd = [TInt, TInt] ~> TInt
        expr' = EApp Pure (Expr tadd $ EName "+" [KExpr $ Expr tadd EBinding])
          [ Expr TBln $ EName "a" [KExpr $ Expr TBln EBinding]
          , Expr TStr $ EName "b" [KExpr $ Expr TStr EBinding]
          ]
        input = Expr TError expr'
        output = Expr TInt expr'
        in reduceExprTest "both-args-wrong-type" input output
          [WrongType TInt TBln, WrongType TInt TStr])

    , (let
        tadd = [TInt, TInt] ~> TInt
        expr' = EApp Pure (Expr tadd $ EName "+" [KExpr $ Expr tadd EBinding])
          [ Expr TBln $ EName "a" [KExpr $ Expr TBln EBinding]
          , Expr TStr $ EName "b" [KExpr $ Expr TStr EBinding]
          , Expr TInt $ EName "c" [KExpr $ Expr TInt EBinding]
          ]
        input = Expr TError expr'
        output = Expr TInt expr'
        in reduceExprTest "wrong-num-args-of-wrong-type" input output
          [WrongNumArgs 2 3, WrongType TInt TBln, WrongType TInt TStr])

    , (let
        tadd = [TInt, TInt] ~> TInt
        expr' = EApp PWrite (Expr tadd $ EName "+" [KExpr $ Expr tadd EBinding])
          [ Expr TBln $ EName "a" [KExpr $ Expr TBln EBinding]
          , Expr TStr $ EName "b" [KExpr $ Expr TStr EBinding]
          , Expr TInt $ EName "c" [KExpr $ Expr TInt EBinding]
          ]
        input = Expr TError expr'
        output = Expr TInt expr'
        in reduceExprTest "wrong-purity-with-too-many-args-of-wrong-type" input output
          [ WrongPurity Pure PWrite
          , WrongNumArgs 2 3
          , WrongType TInt TBln
          , WrongType TInt TStr])

    , (let
        expr' = EApp PWrite (Expr TStr $ EName "a" [KExpr $ Expr TInt EBinding])
          [ Expr TBln $ EName "b" [KExpr $ Expr TBln EBinding] ]
        input = Expr TError expr'
        output = Expr TError expr'
        in reduceExprTest "non-applicable" input output
          [ NonApplicable TStr ])

    ]
  ]
