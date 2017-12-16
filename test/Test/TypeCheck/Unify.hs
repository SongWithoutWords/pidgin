module Test.TypeCheck.Unify(tests) where

import Data.Map as M

import Test.Tasty
import Test.Tasty.HUnit

import TypeCheck.Unify

type SubstitutionList = [(TVar, Type)]

unifyTest :: String -> [Constraint] -> SubstitutionList -> [Error] -> TestTree
unifyTest name constraints subs errors =
  let result = unify constraints
  in testGroup name
    [ testCase "substitutions" $ fst result @?= M.fromList subs
    , testCase "errors" $ snd result @?= errors
    ]

tests :: TestTree
tests = testGroup "Unification"
  [ unifyTest "empty" [] [] []

  , unifyTest "single"
    [TVar 0 :$= TInt]
    [(0, TInt)]
    []

  , unifyTest "simple sub"
    [TVar 0 :$= TVar 1, TVar 1 :$= TInt]
    [(0, TInt), (1, TInt)]
    []

  , unifyTest "multiple subs"
    [TVar 0 :$= TVar 1, TVar 1 :$= TVar 2, TVar 2 :$= TInt]
    [(0, TInt), (1, TInt), (2, TInt)]
    []

  , unifyTest "lambda"
    [TVar 0 :$= TFunc Pure [TVar 1] (TVar 2), TVar 1 :$= TVar 2, TVar 2 :$= TInt]
    [(0, TFunc Pure [TInt] TInt), (1, TInt), (2, TInt)]
    []
  ]

