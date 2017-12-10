{-# language QuasiQuotes #-}
module Test.TypeCheck(tests) where

import Data.String.QQ
import Control.Monad(unless)

import Test.Tasty
import Test.Tasty.HUnit

import qualified Test.TypeCheck.Unify as Unify

import Ast.A3Typed
import Ast.A2Constrained.Error
import Transforms
import Util.MultiMap
import Util.Preface

test :: String -> [(Name, Unit)] -> Errors -> TestTree
test src = namedTest src src

namedTest :: String -> String -> [(Name, Unit)] -> Errors -> TestTree
namedTest name src units errors =
  let (ast, errors') = lexParseCheck src
  in testCase name $ assertAst units ast >> assertErrors errors errors'

test' :: String -> ([(Name, Unit)] -> Errors -> Bool) -> TestTree
test' src = namedTest' src src

namedTest' :: String -> String -> ([(Name, Unit)] -> Errors -> Bool) -> TestTree
namedTest' name src condition =
  let (ast, errors') = lexParseCheck src
  in testCase name $ unless (condition (multiToAscList ast) errors') $ assertFailure $
    "test failed with ast:\n" ++ show ast ++ "\nand errors:\n" ++ show errors'

errorTest :: String -> Errors -> TestTree
errorTest src = namedErrorTest src src

namedErrorTest :: String -> String -> Errors -> TestTree
namedErrorTest name src errors =
  let (_, errors') = lexParseCheck src
  in testCase name $ assertErrors errors errors'

assertAst :: [(Name, Unit)] -> Ast -> Assertion
assertAst units ast' =
  let ast = multiFromList units
  in unless (ast' == ast) $ assertFailure $
    "expected ast:\n" ++ show ast ++ "\nbut got:\n" ++ show ast'

assertErrors :: Errors -> Errors -> Assertion
assertErrors errors errors' =
  unless (errors' == errors) $ assertFailure $
    "expected errors:\n" ++ show errors ++ "\nbut got:\n" ++ show errors'

tests :: TestTree
tests = testGroup "type check"
  [ Unify.tests

  , testGroup "simple tests"
    [ namedTest "empty string" "" [] []

    , test "Bln a = true"
      [("a", UVar $ Var Imut TBln $ Expr TBln $ EVal $ VBln True)]
      []

    , test "Bln a = false"
      [("a", UVar $ Var Imut TBln $ Expr TBln $ EVal $ VBln False)]
      []

    , test "Bln a = 5"
      [("a", UVar $ Var Imut TBln $ Expr TInt $ EVal $ VInt 5)]
      [failedToUnify TBln TInt]

    , test "Int a = true"
      [("a", UVar $ Var Imut TInt $ Expr TBln $ EVal $ VBln True)]
      [failedToUnify TInt TBln]

    , test "$ a = b"
      [("a", UVar $ Var Imut TError $ Expr TError $ EName "b")]
      [UnknownId "b"]
    ]

  , testGroup "recursive definitions"
    [ let
      condition
        [("a", (UVar (Var Imut (TVar a) (Expr (TVar b) (EName "a")))))]
        [FailedToInferType (TVar c), FailedToInferType (TVar d)]
        = alleq [a, b, c, d]
      condition _ _ = False
      in test' "$ a = a" condition

    , let
      condition
        [ ("a", UVar (Var Imut (TVar a) (Expr (TVar b) (EName "b"))))
        , ("b", UVar (Var Imut (TVar c) (Expr (TVar d) (EName "a"))))
        ]
        [ FailedToInferType (TVar e), FailedToInferType (TVar f)
        , FailedToInferType (TVar g), FailedToInferType (TVar h)
        ]
        = alleq [a, b, c, c, d, e, f, g, h]
      condition _ _ = False
      in test' "$ a = b; $ b = a" condition

    , let
      condition
        [ ("a", UVar (Var Imut (TVar a) (Expr (TVar b) (EName "b"))))
        , ("b", UVar (Var Imut (TVar c) (Expr (TVar d) (EName "b"))))
        ]
        [ FailedToInferType (TVar e), FailedToInferType (TVar f)
        , FailedToInferType (TVar g), FailedToInferType (TVar h)
        ]
        = alleq [a, b, c, c, d, e, f, g, h]
      condition _ _ = False
      in test' "$ a = b; $ b = b" condition

    , let
      condition
        [ ("a", UVar (Var Imut (TVar a) (Expr (TVar b) (EName "b"))))
        , ("b", UVar (Var Imut (TVar c) (Expr (TVar d) (EName "c"))))
        , ("c", UVar (Var Imut (TVar e) (Expr (TVar f) (EName "a"))))
        ]
        [ FailedToInferType (TVar g), FailedToInferType (TVar h)
        , FailedToInferType (TVar i), FailedToInferType (TVar j)
        , FailedToInferType (TVar k), FailedToInferType (TVar l)
        ]
        = alleq [a, b, c, c, d, e, f, g, h, i, j, k, l]
      condition _ _ = False
      in test' "$ a = b; $ b = c; $ c = a" condition

    , let
      condition
        [ ("a", UVar (Var Imut (TVar a) (Expr (TVar b) (EName "b"))))
        , ("b", UVar (Var Imut (TVar c) (Expr (TVar d) (EName "c"))))
        , ("c", UVar (Var Imut (TVar e) (Expr (TVar f) (EName "b"))))
        ]
        [ FailedToInferType (TVar g), FailedToInferType (TVar h)
        , FailedToInferType (TVar i), FailedToInferType (TVar j)
        , FailedToInferType (TVar k), FailedToInferType (TVar l)
        ]
        = alleq [a, b, c, c, d, e, f, g, h, i, j, k, l]
      condition _ _ = False
      in test' "$ a = b; $ b = c; $ c = b" condition

    , let b = (Var Imut TInt $ Expr TInt $ EBinOp Add
            (Expr TInt $ EName "b")
            (Expr TInt $ EName "a"))
      in test "$ a = 1; $ b = b + a"
        [ ("a", UVar $ Var Imut TInt $ Expr TInt $ EVal $ VInt 1)
        , ("b", UVar b)
        ]
        [ RecursiveVariableDefinition $ Named "b" b]
    ]

  , errorTest "$ a = true; $ a = false; $ b = a"
    [CompetingDefinitions]

  , errorTest "$ a = true; a() => true; $ b = a"
    [CompetingDefinitions]

  , test "$ a = true; $ b = a"
    [ ("a", UVar $ Var Imut TBln $ Expr TBln $ EVal $ VBln True)
    , ("b", UVar $ Var Imut TBln $ Expr TBln $ EName "a")]
    []

  , test "$ a = b; $ b = true"
    [ ("a", UVar $ Var Imut TBln $ Expr TBln $ EName "b")
    , ("b", UVar $ Var Imut TBln $ Expr TBln $ EVal $ VBln True)]
    []

  , errorTest "$ a = 5; Bln b = a"
    [failedToUnify TBln TInt]

  , test "$ a = 5; $ b = a; $ c = b"
    [ ("a", UVar $ Var Imut TInt $ Expr TInt $ EVal $ VInt 5)
    , ("b", UVar $ Var Imut TInt $ Expr TInt $ EName "a")
    , ("c", UVar $ Var Imut TInt $ Expr TInt $ EName "b")]
    []

  , test "Bln a = b; $ b = c; $ c = 5"
    [ ("a", UVar $ Var Imut TBln $ Expr TInt $ EName "b")
    , ("b", UVar $ Var Imut TInt $ Expr TInt $ EName "c")
    , ("c", UVar $ Var Imut TInt $ Expr TInt $ EVal $ VInt 5)]
    [failedToUnify TBln TInt]


  -- TypeCheck operator tests
  , test "$ a = 3 + 7"
    [("a", UVar $ Var Imut TInt $ Expr TInt
       $ EBinOp Add (Expr TInt $ EVal $ VInt 3) (Expr TInt $ EVal $ VInt 7))]
    []

  , test "$ a = b + c; $ b = 3; $ c = 7"
    [ ("a", UVar $ Var Imut TInt $ Expr TInt
        $ EBinOp Add (Expr TInt $ EName "b") (Expr TInt $ EName "c"))
    , ("b", UVar $ Var Imut TInt $ Expr TInt $ EVal $ VInt 3)
    , ("c", UVar $ Var Imut TInt $ Expr TInt $ EVal $ VInt 7)]
    []

  , test "$ a = 1 if true else 0"
    [("a", UVar $ Var Imut TInt $ Expr TInt
       $ EIf (Cond $ Expr TBln $ EVal $ VBln True)
        (Expr TInt $ EVal $ VInt 1)
        (Expr TInt $ EVal $ VInt 0))
    ]
    []

  , test "$ a = 1 if \"true\" else 0"
    [("a", UVar $ Var Imut TInt $ Expr TInt
       $ EIf (Cond $ Expr TStr $ EVal $ VStr "true")
         (Expr TInt $ EVal $ VInt 1)
         (Expr TInt $ EVal $ VInt 0))]
    [failedToUnify TBln TStr]

  , test "$ a = 1 if true else \"zero\""
    [("a", UVar $ Var Imut TInt $ Expr TInt
       $ EIf (Cond $ Expr TBln $ EVal $ VBln True)
         (Expr TInt $ EVal $ VInt 1)
         (Expr TStr $ EVal $ VStr "zero"))]
    [failedToUnify TInt TStr]

  , errorTest "$ a = 5(1)"
    [NonApplicable TInt, FailedToInferType $ TVar 0, FailedToInferType $ TVar 0]

  , errorTest "$ a = (3 + 2)(1)"
    [NonApplicable TInt, FailedToInferType $ TVar 0, FailedToInferType $ TVar 0]

  , namedTest "one explicit" [s|
one() -> Int => 1
$ a = one()
|]
    [ ("one", UFunc $ Func (Sig Pure [] TInt)
        $ Block [] (Just $ Expr TInt $ EVal $ VInt 1))
    , ("a", UVar $ Var Imut TInt
        $ Expr TInt $ EApp $ App (Expr (TFunc Pure [] TInt) $ EName "one") $ Args Pure [])
    ]
    []

  , namedErrorTest "one explicit, wrong return type"
    "one() -> Int => \"one\""
    [failedToUnify TInt TStr]

  , namedErrorTest "one implicit, wrong num args"
    "one() => 1\n\
    \$ a = one(1)"
    [WrongNumArgs {numArgsRequired = 0, numArgsFound = 1}]

  , namedTest "inc explicit" [s|
inc(Int x) -> Int => x + 1
$ a = inc(1)
|]
    [ ("inc", UFunc $ Func
        (Sig Pure [Param Imut TInt "x"] TInt) $ Block []
          (Just $ Expr TInt $ EBinOp Add (Expr TInt $ EName "x") $ Expr TInt $ EVal $ VInt 1))
    , ("a", UVar $ Var Imut TInt
        $ Expr TInt $ EApp $ App (Expr (TFunc Pure [TInt] TInt) $ EName "inc")
          $ Args Pure [Expr TInt $ EVal $ VInt 1])
    ]
    []

  , namedTest "inc explicit, nested calls" [s|
inc(Int x) -> Int => x + 1
$ a = inc(inc(1))
|]
    [ ("inc", UFunc $ Func
        (Sig Pure [Param Imut TInt "x"] TInt ) $ Block []
          (Just $ Expr TInt $ EBinOp Add (Expr TInt $ EName "x") $ Expr TInt $ EVal $ VInt 1))
    , ("a", UVar $ Var Imut TInt $ Expr TInt
        $ EApp $ App (Expr (TFunc Pure [TInt] TInt) $ EName "inc")
          $ Args Pure [Expr TInt $ EApp $ App (Expr (TFunc Pure [TInt] TInt) $ EName "inc")
            $ Args Pure [Expr TInt $ EVal $ VInt 1]])
    ]
    []

  , namedErrorTest "inc explicit, wrong return type"
    [s|inc(Int x) -> Int => "one"|]
    [failedToUnify TInt TStr]

  , namedErrorTest "inc explicit, wrong num args (a)" [s|
inc(Int x) -> Int => x + 1
$ a = inc()
|]
    [WrongNumArgs {numArgsRequired = 1, numArgsFound = 0}]

  , namedErrorTest "inc explicit, wrong num args (b)" [s|
inc(Int x) -> Int => x + 1
$ a = inc(1, 2)
|]
    [WrongNumArgs {numArgsRequired = 1, numArgsFound = 2}]

  , namedTest "inc implicit" [s|
inc(Int x) => x + 1
$ a = inc(1)
|]
    [ ("inc", UFunc $ Func
        (Sig Pure [Param Imut TInt "x"] TInt) $ Block []
        (Just $ Expr TInt $ EBinOp Add (Expr TInt $ EName "x") $ Expr TInt $ EVal $ VInt 1)
      )
    , ("a", UVar $ Var Imut TInt $ Expr TInt
        $ EApp $ App (Expr (TFunc Pure [TInt] TInt) $ EName "inc")
          $ Args Pure [Expr TInt $ EVal $ VInt 1])
    ]
    []

  , namedTest "inc implicit, wrong arg type" [s|
inc(Int x) => x + 1
$ a = inc("one")
|]
    [ ("inc", UFunc $ Func
        (Sig Pure [Param Imut TInt "x"] TInt) $ Block []
        (Just $ Expr TInt $ EBinOp Add (Expr TInt $ EName "x") $ Expr TInt $ EVal $ VInt 1)
      )
    , ("a", UVar $ Var Imut TInt
        $ Expr TInt $ EApp $ App (Expr (TFunc Pure [TInt] TInt) $ EName "inc")
          $ Args Pure [Expr TStr $ EVal $ VStr "one"])
    ]
    [failedToUnify TInt TStr]

  , namedTest "inc implicit, local var" [s|
inc(Int x) =>
    $ one = 1
    x + one

$ a = inc(1)
|]
    [ ("inc", UFunc $ Func
        (Sig Pure [Param Imut TInt "x"] TInt ) $ Block
        [SVar $ Named "one" $ Var Imut TInt (Expr TInt $ EVal $ VInt 1) ]
        (Just$ Expr TInt $ EBinOp Add (Expr TInt $ EName "x") (Expr TInt $ EName "one")))
    , ("a", UVar $ Var Imut TInt
        $ Expr TInt $ EApp $ App (Expr (TFunc Pure [TInt] TInt) $ EName "inc")
          $ Args Pure [Expr TInt $ EVal $ VInt 1])
    ]
    []
  ]

