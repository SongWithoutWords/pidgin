module Test.X.Reduce(tests) where

import Data.Set as S
-- import Data.Map as M

import Test.Tasty
import Test.Tasty.HUnit

import X.Ast
import X.Error
import X.TypeCheck.Reduce

import Util.MultiMap

-- type SubstitutionList = [(TVar, Type)]

-- unifyTest :: String -> [Constraint] -> SubstitutionList -> [Error] -> TestTree
-- unifyTest name constraints subs errors =
--   let (subsActual, errorsActual) = unify constraints
--   in testGroup name
--     [ testCase "substitutions" $ subsActual @?= M.fromList subs
--     , testCase "errors" $ errorsActual @?= S.fromList errors
--     ]

-- reduceTest :: String -> Ast -> Ast -> TestTree
-- reduceTest name input expected =
--   let result = reduceExpr input
--   in testCase name $ result @?= expected

reduceExprTest :: String -> Expr -> Expr -> Errors -> TestTree
reduceExprTest name input expr errs =
  let (expr', errs') = runReduce $ reduceExpr input
  in testGroup name
    [ testCase "expr" $ expr' @?= expr
    , testCase "errors" $ errs' @?= errs]

-- test :: (Eq a, Show a) => String -> a -> a -> TestTree
-- test name input expected =
--   let result = reduceExpr input
--   in testCase name $ result @?= expected

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

      in reduceExprTest "select-namespace" input output S.empty)

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

     in reduceExprTest "select-namespace-nested" input output)

  , (let
      members = multiFromList
        [ ("x", MVar Pub TFlt)
        , ("y", MVar Pub TFlt)
        ]
      vector = TData "Vector" members
      expr' = ESelect (Expr vector $ EName "x" [KExpr $ Expr vector EBinding]) "y" []
      input = Expr TError expr'
      output = Expr TFlt expr'
    in reduceExprTest "select-member" input output S.empty)


  ]
