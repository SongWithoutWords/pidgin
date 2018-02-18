module Test.X.Reduce(tests) where

import qualified Data.Set as S
-- import Data.Map as M

import Test.Tasty
import Test.Tasty.HUnit

import qualified Ast.A1PostParse as A1
import X.Ast
import X.Error
import X.TypeCheck.Reduce

import X.TypeCheck.ConstraintGen

import Util.MultiMap

-- reduceExprTest :: String -> Expr -> Expr -> [Error] -> TestTree
-- reduceExprTest name input expr errorList =
--   let
--     errs = S.fromList errorList
--     (expr', errs') = runReduce $ reduceExpr input
--   in testGroup name
--     [ testCase "expr" $ expr' @?= expr
--     , testCase "errors" $ errs' @?= errs]

reduceAstTest :: String -> A1.Ast -> Ast -> TestTree
reduceAstTest name input expected =
  let output = constrainAst input
  in testCase name $ output @?= expected


tests :: TestTree
tests = testGroup "reduce"
  [ (let
        input = multiFromList
          [("a", A1.UVar $ A1.Var A1.Imt Nothing $ A1.EVal $ A1.VInt 1)]
        output = Ast
          { namespace = multiFromList [("a", UVar (VarId 0))]
          , functions = fromList []
          , types = fromList []
          , vars = fromList [Var "a" $ Expr TInt $ EVal $ VInt 1]
          }
      in
        reduceAstTest "single-var" input output)

  , (let
      input = multiFromList
        [ ("a", A1.UVar $ A1.Var A1.Imt Nothing $ A1.EVal $ A1.VInt 1)
        , ("b", A1.UVar $ A1.Var A1.Imt Nothing $ A1.EVal $ A1.VBln False)
        ]
      output = Ast
        { namespace = multiFromList [("a", UVar (VarId 0)), ("b", UVar (VarId 1))]
        , functions = fromList []
        , types = fromList []
        , vars = fromList
          [ Var "a" $ Expr TInt $ EVal $ VInt 1
          , Var "b" $ Expr TBln $ EVal $ VBln False]
        }
    in
      reduceAstTest "two-vars" input output)

  , (let
      input = multiFromList
        [ ("a", A1.UVar $ A1.Var A1.Imt Nothing $ A1.EName "b")
        , ("b", A1.UVar $ A1.Var A1.Imt Nothing $ A1.EName "a")
        ]
      output = Ast
        { namespace = multiFromList [("a", UVar (VarId 0)), ("b", UVar (VarId 1))]
        , functions = fromList []
        , types = fromList []
        , vars = fromList
          [ Var "a" $ Expr TInt $ EName $ UVar $ VarId 1
          , Var "b" $ Expr TInt $ EVal $ VInt 1]
        }
    in
      reduceAstTest "variable-forward-reference" input output)

  , (let
      input = multiFromList
        [ ("a", A1.UVar $ A1.Var A1.Imt Nothing $ A1.EVal $ A1.VInt 1)
        , ("b", A1.UVar $ A1.Var A1.Imt Nothing $ A1.EName "a")
        ]
      output = Ast
        { namespace = multiFromList [("a", UVar (VarId 0)), ("b", UVar (VarId 1))]
        , functions = fromList []
        , types = fromList []
        , vars = fromList
          [ Var "a" $ Expr TInt $ EVal $ VInt 1
          , Var "b" $ Expr TInt $ EName $ UVar $ VarId 0]
        }
    in
      reduceAstTest "variable-back-reference" input output)
  ]
  -- [ (let
  --     unitsA = multiFromList
  --       [ ("b", UNamespace unitsB)
  --       , ("c", UData $ multiFromList [])
  --       ]
  --     unitsB = multiFromList
  --       [ ("d", UNamespace $ multiFromList [])
  --       , ("e", UData $ multiFromList [])
  --       ]
  --     input =
  --       Expr TUnknown $ ESelect
  --         (Expr TUnknown $ EName "a" [KNamespace unitsA])
  --         "b" []
  --     output = Expr TUnknown $ EName "a.b" [KNamespace unitsB]

  --     in reduceExprTest "select-namespace" input output [])

  -- , (let
  --     unitsA = multiFromList
  --       [ ("b", UNamespace unitsB)
  --       , ("x", UData $ multiFromList [])
  --       ]
  --     unitsB = multiFromList
  --       [ ("c", UNamespace unitsC)
  --       , ("y", UData $ multiFromList [])
  --       ]
  --     unitsC = multiFromList
  --       [ ("d", UNamespace $ multiFromList [])
  --       , ("z", UData $ multiFromList [])
  --       ]
  --     input =
  --       Expr TUnknown $ ESelect
  --         (Expr TUnknown $ ESelect
  --           (Expr TUnknown $ EName "a" [KNamespace unitsA])
  --           "b" []
  --         )
  --       "c" []
  --     output = Expr TUnknown $ EName "a.b.c" [KNamespace unitsC]

  --    in reduceExprTest "select-namespace-nested" input output [])

  -- , (let
  --     members = multiFromList
  --       [ ("x", MVar Pub TFlt)
  --       , ("y", MVar Pub TFlt)
  --       ]
  --     vector = TData "Vector" members
  --     expr' = ESelect (Expr vector $ EName "x" [KExpr $ Expr vector EBinding]) "y" []
  --     input = Expr TUnknown expr'
  --     output = Expr TFlt expr'
  --   in reduceExprTest "select-member" input output [])

  -- , testGroup "app"
  --   [ (let
  --     tadd = [TInt, TInt] ~> TInt
  --     expr' = EApp Pure (Expr tadd $ EName "+" [KExpr $ Expr tadd EBinding])
  --       [ Expr TInt $ EName "a" [KExpr $ Expr TInt EBinding]
  --       , Expr TInt $ EName "b" [KExpr $ Expr TInt EBinding]
  --       ]
  --     input = Expr TUnknown expr'
  --     output = Expr TInt expr'
  --     in reduceExprTest "simple-app" input output [])

  --   , (let
  --     tadd = [TInt, TInt] ~> TInt
  --     expr' = EApp PWrite (Expr tadd $ EName "+" [KExpr $ Expr tadd EBinding])
  --       [ Expr TInt $ EName "a" [KExpr $ Expr TInt EBinding]
  --       , Expr TInt $ EName "b" [KExpr $ Expr TInt EBinding]
  --       ]
  --     input = Expr TUnknown expr'
  --     output = Expr TInt expr'
  --     in reduceExprTest "wrong-purity" input output [WrongPurity Pure PWrite])

  --   , (let
  --       tadd = [TInt, TInt] ~> TInt
  --       expr' = EApp Pure (Expr tadd $ EName "+" [KExpr $ Expr tadd EBinding])
  --         [ Expr TInt $ EName "a" [KExpr $ Expr TInt EBinding]
  --         , Expr TInt $ EName "b" [KExpr $ Expr TInt EBinding]
  --         , Expr TInt $ EName "c" [KExpr $ Expr TInt EBinding]
  --         ]
  --       input = Expr TUnknown expr'
  --       output = Expr TInt expr'
  --       in reduceExprTest "wrong-num-args" input output [WrongNumArgs 2 3])

  --   , (let
  --       tadd = [TInt, TInt] ~> TInt
  --       expr' = EApp Pure (Expr tadd $ EName "+" [KExpr $ Expr tadd EBinding])
  --         [ Expr TBln $ EName "a" [KExpr $ Expr TBln EBinding]
  --         , Expr TInt $ EName "b" [KExpr $ Expr TInt EBinding]
  --         ]
  --       input = Expr TUnknown expr'
  --       output = Expr TInt expr'
  --       in reduceExprTest "first-arg-wrong-type" input output [WrongType TInt TBln])

  --   , (let
  --       tadd = [TInt, TInt] ~> TInt
  --       expr' = EApp Pure (Expr tadd $ EName "+" [KExpr $ Expr tadd EBinding])
  --         [ Expr TInt $ EName "a" [KExpr $ Expr TInt EBinding]
  --         , Expr TStr $ EName "b" [KExpr $ Expr TStr EBinding]
  --         ]
  --       input = Expr TUnknown expr'
  --       output = Expr TInt expr'
  --       in reduceExprTest "second-arg-wrong-type" input output [WrongType TInt TStr])

  --   , (let
  --       tadd = [TInt, TInt] ~> TInt
  --       expr' = EApp Pure (Expr tadd $ EName "+" [KExpr $ Expr tadd EBinding])
  --         [ Expr TBln $ EName "a" [KExpr $ Expr TBln EBinding]
  --         , Expr TStr $ EName "b" [KExpr $ Expr TStr EBinding]
  --         ]
  --       input = Expr TUnknown expr'
  --       output = Expr TInt expr'
  --       in reduceExprTest "both-args-wrong-type" input output
  --         [WrongType TInt TBln, WrongType TInt TStr])

  --   , (let
  --       tadd = [TInt, TInt] ~> TInt
  --       expr' = EApp Pure (Expr tadd $ EName "+" [KExpr $ Expr tadd EBinding])
  --         [ Expr TBln $ EName "a" [KExpr $ Expr TBln EBinding]
  --         , Expr TStr $ EName "b" [KExpr $ Expr TStr EBinding]
  --         , Expr TInt $ EName "c" [KExpr $ Expr TInt EBinding]
  --         ]
  --       input = Expr TUnknown expr'
  --       output = Expr TInt expr'
  --       in reduceExprTest "wrong-num-args-of-wrong-type" input output
  --         [WrongNumArgs 2 3, WrongType TInt TBln, WrongType TInt TStr])

  --   , (let
  --       tadd = [TInt, TInt] ~> TInt
  --       expr' = EApp PWrite (Expr tadd $ EName "+" [KExpr $ Expr tadd EBinding])
  --         [ Expr TBln $ EName "a" [KExpr $ Expr TBln EBinding]
  --         , Expr TStr $ EName "b" [KExpr $ Expr TStr EBinding]
  --         , Expr TInt $ EName "c" [KExpr $ Expr TInt EBinding]
  --         ]
  --       input = Expr TUnknown expr'
  --       output = Expr TInt expr'
  --       in reduceExprTest "wrong-purity-with-too-many-args-of-wrong-type" input output
  --         [ WrongPurity Pure PWrite
  --         , WrongNumArgs 2 3
  --         , WrongType TInt TBln
  --         , WrongType TInt TStr])

  --   , (let
  --       expr' = EApp PWrite (Expr TStr $ EName "a" [KExpr $ Expr TInt EBinding])
  --         [ Expr TBln $ EName "b" [KExpr $ Expr TBln EBinding] ]
  --       input = Expr TUnknown expr'
  --       output = Expr TError expr'
  --       in reduceExprTest "non-applicable" input output
  --         [ NonApplicable TStr ])
  --   ]

  -- , testGroup "if"
  --   [ (let
  --     expr' = EIf
  --       (Expr TBln $ EName "a" [KExpr $ Expr TBln EBinding])
  --       [Expr TInt $ EName "b" [KExpr $ Expr TInt EBinding]]
  --       [Expr TInt $ EName "c" [KExpr $ Expr TInt EBinding]]
  --     input = Expr TUnknown expr'
  --     output = Expr TInt expr'
  --     in reduceExprTest "simple" input output [])

  --   , (let
  --     expr' = EIf
  --       (Expr TStr $ EName "a" [KExpr $ Expr TStr EBinding])
  --       [Expr TInt $ EName "b" [KExpr $ Expr TInt EBinding]]
  --       [Expr TInt $ EName "c" [KExpr $ Expr TInt EBinding]]
  --     input = Expr TUnknown expr'
  --     output = Expr TInt expr'
  --     in reduceExprTest "wrong-condition-type" input output [WrongType TBln TStr])

  --   , (let
  --     expr' = EIf
  --       (Expr TBln $ EName "a" [KExpr $ Expr TBln EBinding])
  --       [Expr TInt $ EName "b" [KExpr $ Expr TInt EBinding]]
  --       [Expr TStr $ EName "c" [KExpr $ Expr TStr EBinding]]
  --     input = Expr TUnknown expr'
  --     output = Expr TInt expr'
  --     in reduceExprTest "mismatched-types" input output [WrongType TInt TStr])

  --   , (let
  --     expr' = EIf
  --       (Expr TBln $ EName "a" [KExpr $ Expr TBln EBinding])
  --       [Expr TStr $ EName "b" [KExpr $ Expr TStr EBinding]]
  --       [Expr TInt $ EName "c" [KExpr $ Expr TInt EBinding]]
  --     input = Expr TUnknown expr'
  --     output = Expr TStr expr'
  --     in reduceExprTest "mismatched-types-b" input output [WrongType TStr TInt])
  --   ]

  -- ]
