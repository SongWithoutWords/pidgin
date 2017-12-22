module Test.TypeCheck.Unify(tests) where

import Data.Set as S
import Data.Map as M

import Test.Tasty
import Test.Tasty.HUnit

import TypeCheck.Unify

type SubstitutionList = [(TVar, Type)]

unifyTest :: String -> [Constraint] -> SubstitutionList -> [Error] -> TestTree
unifyTest name constraints subs errors =
  let (subsActual, errorsActual) = unify constraints
  in testGroup name
    [ testCase "substitutions" $ subsActual @?= M.fromList subs
    , testCase "errors" $ errorsActual @?= S.fromList errors
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

  , unifyTest "simple overload"
    [TFunc Pure [TInt, TInt] (TVar 0) :$= TOver
     [ TFunc Pure [TInt, TInt] TInt
     , TFunc Pure [TFlt, TFlt] TFlt]]
    [(0, TInt)]
    []

  , unifyTest "another overload"
    [ TVar 0 :$= TOver [TFunc Pure [TInt, TInt] TInt, TFunc Pure [TFlt, TFlt] TFlt]
    , TFunc Pure [TInt, TInt] (TVar 1) :$= TVar 0
    ]
    [(0, TFunc Pure [TInt, TInt] TInt), (1, TInt)]
    []

  , unifyTest "overload selection with subtyping"
    [ TVar 0 :$= TOver [TFunc Pure [TInt, TInt] TInt, TFunc Pure [TFlt, TFlt] TFlt]
    , TFunc Pure [TInt, TFlt] (TVar 1) :$= TVar 0
    ]
    [(0, TFunc Pure [TFlt, TFlt] TFlt), (1, TFlt)]
    []
  ]

