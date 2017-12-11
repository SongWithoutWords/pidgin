module Test.TypeCheck.Unify(tests) where

import Data.Map as M

import Test.Tasty
import Test.Tasty.HUnit

import TypeCheck.Unify

type SubstitutionList = [(Word, Type)]

unifyTest :: String -> [Constraint] -> SubstitutionList -> [Error] -> TestTree
unifyTest name constraints subs errors =
  let result = unify constraints
  in testGroup name
    [ testCase "substitutions" $ fst result @?= M.fromList subs
    , testCase "errors" $ snd result @?= errors
    ]

tests :: TestTree
tests = testGroup "Unification"
  [ unifyTest "00 - empty" [] [] []

  , unifyTest "01 - single"
    [TVar 0 :< TInt]
    [(0, TInt)]
    []

  , unifyTest "02 - simple sub"
    [TVar 0 :< TVar 1, TVar 1 :< TInt]
    [(0, TInt), (1, TInt)]
    []

  , unifyTest "03 - multiple subs"
    [TVar 0 :< TVar 1, TVar 1 :< TVar 2, TVar 2 :< TInt]
    [(0, TInt), (1, TInt), (2, TInt)]
    []

  , unifyTest "04 - lambda"
    [TVar 0 :< TFunc Pure [TVar 1] (TVar 2), TVar 1 :< TVar 2, TVar 2 :< TInt]
    [(0, TFunc Pure [TInt] TInt), (1, TInt), (2, TInt)]
    []

  , unifyTest "05 - lambda equality one sided"
    [TFunc Pure [TVar 0] (TVar 1) :< TFunc Pure [TBln] TInt]
    [(0, TBln), (1, TInt)]
    []

  , unifyTest "06 - lambda two sided"
    [TFunc Pure [TVar 0] TInt :< TFunc Pure [TBln] (TVar 1)]
    [(0, TBln), (1, TInt)]
    []

  , unifyTest "07 - typeVar :< lambda"
    [ TVar 0 :< TFunc Pure [TVar 1] TInt
    , TVar 0 :< TFunc Pure [TBln] (TVar 2)
    ]
    [(0, TFunc Pure [TBln] TInt), (1, TBln), (2, TInt)]
    []

  , unifyTest "08 - inc constraints"
    [ TInt :< TVar 1
    , TInt :< TInt
    , TInt :< TVar 2
    , TVar 0 :< TFunc Pure [TVar 1] (TVar 2)
    , TVar 0 :< TFunc Pure [TInt] (TVar 3)
    ]
    [ (0, TFunc Pure [TInt] TInt)
    , (1, TInt)
    , (2, TInt)
    , (3, TInt)
    ]
    []
  ]

