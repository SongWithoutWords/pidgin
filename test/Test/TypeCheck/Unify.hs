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

  , unifyTest "overload-ish"
    [TFunc Pure [TInt, TInt] (TVar 0) :$= TOver 1 [ TFunc Pure [TInt, TInt] TInt]]
    [(0, TInt), (1, TFunc Pure [TInt, TInt] TInt)]
    []

  , unifyTest "simple overload"
    [TFunc Pure [TInt, TInt] (TVar 0) :$= TOver 1
     [ TFunc Pure [TInt, TInt] TInt
     , TFunc Pure [TFlt, TFlt] TFlt]]
    [(0, TInt), (1, TFunc Pure [TInt, TInt] TInt)]
    []

  , unifyTest "overload selection with subtyping"
    [ TFunc Pure [TInt, TFlt] (TVar 0) :$= TOver 1
      [ TFunc Pure [TInt, TInt] TInt
      , TFunc Pure [TFlt, TFlt] TFlt]
    ]
    [(0, TFlt), (1, TFunc Pure [TFlt, TFlt] TFlt)]
    []

  , unifyTest "equally viable overload 1"
    [ TVar 0 :$= TOver 1 [TBln, TFlt] ]
    []
    [EquallyViableOverloads (TVar 0) $ S.fromList [TBln, TFlt]]

  , unifyTest "array construction"
    [TFunc Pure [TInt, TInt] (TVar 0) :$= TFunc Pure [TInt, TVar 1] (TArray $ TVar 1)]
    [(0, TArray TInt), (1, TInt)]
    []

  , unifyTest "array access"
    [TFunc Pure [TArray TInt, TInt] (TVar 0) :$= TFunc Pure [TArray $ TVar 1, TInt] (TVar 1)]
    [(0, TInt), (1, TInt)]
    []
  ]

