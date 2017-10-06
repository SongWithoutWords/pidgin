module Test.TypeCheck.Unify(unifyTests) where

import Data.Map as M

import Test.Tasty
import Test.Tasty.HUnit

import TypeCheck.Unify

unifyTests :: TestTree
unifyTests = testGroup "Unification"
  [ test00
  , test01
  , test02
  , test03
  , test04
  , test05
  , test06
  , test07
  , test08
  ]

unifyTest :: String -> [Constraint] -> Substitutions -> TestTree
unifyTest name c s = testCase name $ unifyConstraints c @?= s

test00 = unifyTest "00 - empty" c s
  where
    c = []; s = M.empty

test01 = unifyTest "01 - single" c s
  where
    c = [TVar 0 := TInt]
    s = M.singleton 0 TInt

test02 = unifyTest "02 - simple sub" c s
  where
    c = [TVar 0 := TVar 1, TVar 1 := TInt]
    s = M.fromList [(0, TInt), (1, TInt)]

test03 = unifyTest "03 - multiple subs" c s
  where
    c = [TVar 0 := TVar 1, TVar 1 := TVar 2, TVar 2 := TInt]
    s = M.fromList [(0, TInt), (1, TInt), (2, TInt)]

test04 = unifyTest "04 - lambda" c s
  where
    c = [TVar 0 := TFunc Pure [TVar 1] (TVar 2), TVar 1 := TVar 2, TVar 2 := TInt]
    s = M.fromList [(0, TFunc Pure [TInt] TInt), (1, TInt), (2, TInt)]

test05 = unifyTest "05 - lambda equality one sided" c s
  where
    c = [TFunc Pure [TVar 0] (TVar 1) := TFunc Pure [TBln] TInt]
    s = M.fromList [(0, TBln), (1, TInt)]

test06 = unifyTest "06 - lambda two sided" c s
  where
    c = [TFunc Pure [TVar 0] TInt := TFunc Pure [TBln] (TVar 1)]
    s = M.fromList [(0, TBln), (1, TInt)]

test07 = unifyTest "07 - typeVar := lambda" c s
  where
    c = [ TVar 0 := TFunc Pure [TVar 1] TInt
        , TVar 0 := TFunc Pure [TBln] (TVar 2) ]
    s = M.fromList [(0, TFunc Pure [TBln] TInt), (1, TBln), (2, TInt)]

test08 = unifyTest "08 - inc constraints" c s
  where
    c = [ TInt := TVar 1
        , TInt := TInt
        , TInt := TVar 2
        , TVar 0 := TFunc Pure [TVar 1] (TVar 2)
        , TVar 0 := TFunc Pure [TInt] (TVar 3)]

    s = M.fromList
        [ (0, TFunc Pure [TInt] TInt)
        , (1, TInt)
        , (2, TInt)
        , (3, TInt)
        ]
