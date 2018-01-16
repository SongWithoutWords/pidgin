{-# language QuasiQuotes #-}
module Test.TypeCheck(tests) where

import Control.Monad(when, unless)
import qualified Data.Set as S
import Data.String.QQ

import Test.Tasty
import Test.Tasty.HUnit

import qualified Test.TypeCheck.Unify as Unify

import Ast.A3Typed
import Ast.A2Constrained.Error
import Ediff
import TypeCheck.Constraint
import Transforms
import Util.MultiMap
import Util.Preface
import Util.PrettyShow

test :: String -> [(Name, Unit)] -> [Error] -> TestTree
test src = namedTest src src

namedTest :: String -> String -> [(Name, Unit)] -> [Error] -> TestTree
namedTest name src units errors =
  let (ast, errors') = lexParseCheck src
  in testGroup name [testAst "ast" name units ast, testErrors "errors" errors errors']

test' :: String -> ([(Name, Unit)] -> [Error] -> Bool) -> TestTree
test' src = namedTest' src src

namedTest' :: String -> String -> ([(Name, Unit)] -> [Error] -> Bool) -> TestTree
namedTest' name src condition =
  let (ast, errors') = lexParseCheck src
  in testCase name
    $ unless (condition (multiToAscList ast) (S.toAscList errors'))
    $ assertFailure $
    "test failed with ast:\n" ++ prettyShow ast ++
    "\nand errors:\n" ++ prettyShow errors'

errorTest :: String -> [Error] -> TestTree
errorTest src = namedErrorTest src src

namedErrorTest :: String -> String -> [Error] -> TestTree
namedErrorTest name src errors =
  let (_, errors') = lexParseCheck src
  in testErrors name errors errors'

testAst :: String -> String -> [(Name, Unit)] -> Ast -> TestTree
testAst name ediffTitle units ast' =
  let ast = multiFromList units
  in askOption $ \(Ediff ediffEnable) -> testCase name $
  unless (ast' == ast) $ do
    when ediffEnable $ ediff ediffTitle (prettyShow ast) (prettyShow ast')
    assertFailure $
      "expected ast:\n" ++ prettyShow ast ++ "\nbut got:\n" ++ prettyShow ast'

testErrors :: String -> [Error] -> Errors -> TestTree
testErrors name errorList errors' =
  let errors = S.fromList errorList
  in testCase name $
    unless (errors' == errors) $ assertFailure $
      "expected errors:\n" ++ prettyShow errors ++
      "\nbut got:\n" ++ prettyShow errors'

tests :: TestTree
tests = testGroup "typecheck"
  [ Unify.tests

  , testGroup "simple tests"
    [ namedTest "empty string" "" [] []

    , test "Bln a = true"
      [("a", UVar $ Var TBln $ Expr TBln $ EVal $ VBln True)]
      []

    , test "Bln a = false"
      [("a", UVar $ Var TBln $ Expr TBln $ EVal $ VBln False)]
      []

    , test "Bln a = 5"
      [("a", UVar $ Var TBln $ Expr TInt $ EVal $ VInt 5)]
      [FailedToUnify $ TBln :$= TInt]

    , test "Int a = true"
      [("a", UVar $ Var TInt $ Expr TBln $ EVal $ VBln True)]
      [FailedToUnify $ TInt :$= TBln]

    , test "$ a = b"
      [("a", UVar $ Var TError $ Expr TError $ EName "b")]
      [UnknownId "b"]

    -- infer me some types, and some mutable ones
    , test "$ a = true"
      [("a", UVar $ Var TBln $ Expr TBln $ EVal $ VBln True)]
      []

    , test "~$ a = true"
      [("a", UVar $ Var (TMut TBln) $ Expr TBln $ EVal $ VBln True)]
      []
    ]

  , testGroup "recursive definitions"
    [ let
      condition
        [("a", (UVar (Var (TVar t) (Expr (TVar u) (EName "a")))))]
        [FailedToInferType (TVar v)]
        = alleq [t, u, v]
      condition _ _ = False
      in test' "$ a = a" condition

    , let
      condition
        [ ("a", UVar (Var (TVar t) (Expr (TVar u) (EName "b"))))
        , ("b", UVar (Var (TVar v) (Expr (TVar w) (EName "a"))))
        ]
        [FailedToInferType (TVar x)]
        = alleq [t, u, v, w, x]
      condition _ _ = False
      in test' "$ a = b; $ b = a" condition

    , let
      condition
        [ ("a", UVar (Var (TVar t) (Expr (TVar u) (EName "b"))))
        , ("b", UVar (Var (TVar v) (Expr (TVar w) (EName "b"))))
        ]
        [FailedToInferType (TVar x)]
        = alleq [t, u, v, w, x]
      condition _ _ = False
      in test' "$ a = b; $ b = b" condition

    , let
      condition
        [ ("a", UVar (Var (TVar t) (Expr (TVar u) (EName "b"))))
        , ("b", UVar (Var (TVar v) (Expr (TVar w) (EName "c"))))
        , ("c", UVar (Var (TVar x) (Expr (TVar y) (EName "a"))))
        ]
        [ FailedToInferType (TVar z)
        ]
        = alleq [t, u, v, w, x, y, z]
      condition _ _ = False
      in test' "$ a = b; $ b = c; $ c = a" condition

    , let
      condition
        [ ("a", UVar (Var (TVar t) (Expr (TVar u) (EName "b"))))
        , ("b", UVar (Var (TVar v) (Expr (TVar w) (EName "c"))))
        , ("c", UVar (Var (TVar x) (Expr (TVar y) (EName "b"))))
        ]
        [FailedToInferType (TVar z)]
        = alleq [t, u, v, w, x, y, z]
      condition _ _ = False
      in test' "$ a = b; $ b = c; $ c = b" condition

    , let b = (Var TInt $ Expr TInt $ EApp (Expr (TFunc Pure [TInt, TInt] TInt) $ EIntr IAdd)
            Pure
            [ Expr TInt $ EName "b"
            , Expr TInt $ EName "a"
            ])
      in test "$ a = 1; $ b = b + a"
        [ ("a", UVar $ Var TInt $ Expr TInt $ EVal $ VInt 1)
        , ("b", UVar b)
        ]
        [ RecursiveVariableDefinition "b"]
    ]

  , errorTest "$ a = true; $ a = false; $ b = a"
    [EquallyViableOverloads TError $ S.fromList [TBln, TBln]]

  , errorTest "$ a = true; a() => true; $ b = a"
    [EquallyViableOverloads TError $ S.fromList [TBln, TFunc Pure [] TBln]]

  , test "$ a = true; $ b = a"
    [ ("a", UVar $ Var TBln $ Expr TBln $ EVal $ VBln True)
    , ("b", UVar $ Var TBln $ Expr TBln $ EName "a")]
    []

  , test "$ a = b; $ b = true"
    [ ("a", UVar $ Var TBln $ Expr TBln $ EName "b")
    , ("b", UVar $ Var TBln $ Expr TBln $ EVal $ VBln True)]
    []

  , errorTest "$ a = 5; Bln b = a"
    [FailedToUnify $ TBln :$= TInt]

  , test "$ a = 5; $ b = a; $ c = b"
    [ ("a", UVar $ Var TInt $ Expr TInt $ EVal $ VInt 5)
    , ("b", UVar $ Var TInt $ Expr TInt $ EName "a")
    , ("c", UVar $ Var TInt $ Expr TInt $ EName "b")]
    []

  , test "Bln a = b; $ b = c; $ c = 5"
    [ ("a", UVar $ Var TBln $ Expr TInt $ EName "b")
    , ("b", UVar $ Var TInt $ Expr TInt $ EName "c")
    , ("c", UVar $ Var TInt $ Expr TInt $ EVal $ VInt 5)]
    [FailedToUnify $ TBln :$= TInt]


  -- TypeCheck operator tests
  , test "$ a = 3 + 7"
    [("a", UVar $ Var TInt $ Expr TInt
           $ EApp (Expr (TFunc Pure [TInt, TInt] TInt) $ EIntr IAdd) Pure
        [Expr TInt $ EVal $ VInt 3, Expr TInt $ EVal $ VInt 7]
     )]
    []

  , test "$ a = b + c; $ b = 3; $ c = 7"
    [ ("a", UVar $ Var TInt $ Expr TInt
        $ EApp (Expr (TFunc Pure [TInt, TInt] TInt) $ EIntr IAdd) Pure
          [Expr TInt $ EName "b", Expr TInt $ EName "c"])
    , ("b", UVar $ Var TInt $ Expr TInt $ EVal $ VInt 3)
    , ("c", UVar $ Var TInt $ Expr TInt $ EVal $ VInt 7)]
    []

  , test "$ a = 1 if true else 0"
    [("a", UVar $ Var TInt $ Expr TInt
       $ EIf (Cond $ Expr TBln $ EVal $ VBln True)
        (Expr TInt $ EVal $ VInt 1)
        (Expr TInt $ EVal $ VInt 0))
    ]
    []

  , test "$ a = 1 if \"true\" else 0"
    [("a", UVar $ Var TInt $ Expr TInt
       $ EIf (Cond $ Expr TStr $ EVal $ VStr "true")
         (Expr TInt $ EVal $ VInt 1)
         (Expr TInt $ EVal $ VInt 0))]
    [FailedToUnify $ TBln :$= TStr]

  , test "$ a = 1 if true else \"zero\""
    [("a", UVar $ Var TInt $ Expr TInt
       $ EIf (Cond $ Expr TBln $ EVal $ VBln True)
         (Expr TInt $ EVal $ VInt 1)
         (Expr TStr $ EVal $ VStr "zero"))]
    [FailedToUnify $ TInt :$= TStr]

  , errorTest "$ a = 5(1)"
    [NonApplicable TInt]

  , errorTest "$ a = (3 + 2)(1)"
    [NonApplicable TInt]

  , namedTest "one explicit" [s|
one() -> Int => 1
$ a = one()
|]
    [ ("one", UFunc $ Func (Sig Pure [] TInt)
        $ Block [] (Just $ Expr TInt $ EVal $ VInt 1))
    , ("a", UVar $ Var TInt
        $ Expr TInt $ EApp
          (Expr (TFunc Pure [] TInt) $ EName "one")
          Pure [])
    ]
    []

  , namedErrorTest "one explicit, wrong return type"
    "one() -> Int => \"one\""
    [FailedToUnify $ TInt :$= TStr]

  , namedErrorTest "one implicit, wrong num args"
    "one() => 1\n\
    \$ a = one(1)"
    [WrongNumArgs {numArgsRequired = 0, numArgsFound = 1}]

  , namedTest "inc explicit" [s|
inc(Int x) -> Int => x + 1
$ a = inc(1)
|]
    [ ("inc", UFunc $ Func
        (Sig Pure [Named "x" $ TInt] TInt) $ Block []
          (Just
            $ Expr TInt
              $ EApp (Expr (TFunc Pure [TInt, TInt] TInt) $ EIntr IAdd) Pure
                [Expr TInt $ EName "x", Expr TInt $ EVal $ VInt 1]))
    , ("a", UVar $ Var TInt
        $ Expr TInt
          $ EApp
            (Expr (TFunc Pure [TInt] TInt) $ EName "inc")
            Pure [Expr TInt $ EVal $ VInt 1])
    ]
    []

  , namedTest "inc explicit, nested calls" [s|
inc(Int x) -> Int => x + 1
$ a = inc(inc(1))
|]
    [ ("inc", UFunc $ Func
        (Sig Pure [Named "x" $ TInt] TInt ) $ Block []
          (Just $ Expr TInt
            $ EApp (Expr (TFunc Pure [TInt, TInt] TInt) $ EIntr IAdd) Pure
              [Expr TInt $ EName "x", Expr TInt $ EVal $ VInt 1]))
    , ("a", UVar $ Var TInt $ Expr TInt
        $ EApp
          (Expr (TFunc Pure [TInt] TInt) $ EName "inc")
          Pure [Expr TInt
            $ EApp
              (Expr (TFunc Pure [TInt] TInt) $ EName "inc")
              Pure [Expr TInt $ EVal $ VInt 1]])
    ]
    []

  , namedErrorTest "inc explicit, wrong return type"
    [s|inc(Int x) -> Int => "one"|]
    [FailedToUnify $ TInt :$= TStr]

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
        (Sig Pure [Named "x" $ TInt] TInt) $ Block []
        (Just $ Expr TInt
          $ EApp (Expr (TFunc Pure [TInt, TInt] TInt) $ EIntr IAdd)
            Pure [Expr TInt $ EName "x", Expr TInt $ EVal $ VInt 1]))

    , ("a", UVar $ Var TInt $ Expr TInt
        $ EApp (Expr (TFunc Pure [TInt] TInt) $ EName "inc")
          Pure [Expr TInt $ EVal $ VInt 1])
    ]
    []

  , namedTest "inc implicit, wrong arg type" [s|
inc(Int x) => x + 1
$ a = inc("one")
|]
    [ ("inc", UFunc $ Func
        (Sig Pure [Named "x" $ TInt] TInt) $ Block []
        (Just $ Expr TInt
          $ EApp (Expr (TFunc Pure [TInt, TInt] TInt) $ EIntr IAdd)
            Pure [Expr TInt $ EName "x", Expr TInt $ EVal $ VInt 1]))

    , ("a", UVar $ Var TInt
        $ Expr TInt $ EApp (Expr (TFunc Pure [TInt] TInt) $ EName "inc")
          Pure [Expr TStr $ EVal $ VStr "one"])
    ]
    [FailedToUnify $ TInt :$= TStr]

  , namedTest "inc implicit, local var" [s|
inc(Int x) =>
    $ one = 1
    x + one

$ a = inc(1)
|]
    [ ("inc", UFunc $ Func
        (Sig Pure [Named "x" $ TInt] TInt ) $ Block
        [SVar $ Named "one" $ Var TInt (Expr TInt $ EVal $ VInt 1) ]
        (Just$ Expr TInt
          $ EApp (Expr (TFunc Pure [TInt, TInt] TInt) $ EIntr IAdd)
            Pure [Expr TInt $ EName "x", Expr TInt $ EName "one"]))
    , ("a", UVar $ Var TInt
        $ Expr TInt $ EApp (Expr (TFunc Pure [TInt] TInt) $ EName "inc")
          Pure [Expr TInt $ EVal $ VInt 1])
    ]
    []

  , namedTest "factorial-explicit" [s|
fact(Int n) -> Int =>
    1 if n <= 1 else n * fact(n - 1)
|]
   [("fact", UFunc $ Func
     (Sig Pure [Named "n" $ TInt] TInt) $ Block []
      $ Just $ Expr TInt
        $ EIf
          (Cond $ Expr TBln $ EApp
            (Expr (TFunc Pure [TInt, TInt] TBln) $ EIntr ILeq)
            Pure [Expr TInt $ EName "n", Expr TInt $ EVal $ VInt 1])
          (Expr TInt $ EVal $ VInt 1)
          (Expr TInt $ EApp
            (Expr (TFunc Pure [TInt, TInt] TInt) $ EIntr IMul) Pure
            [Expr TInt $ EName "n"
            , Expr TInt $ EApp
              (Expr (TFunc Pure [TInt] TInt) $ EName "fact") Pure
                [Expr TInt $ EApp (Expr (TFunc Pure [TInt, TInt] TInt) $ EIntr ISub) Pure
                  [Expr TInt $ EName "n", Expr TInt $ EVal $ VInt 1]
                ]
            ]
          )
     )
   ]
   []

  , namedTest "factorial-implicit" [s|
fact(Int n) =>
    1 if n <= 1 else n * fact(n - 1)
|]
   [("fact", UFunc $ Func
     (Sig Pure [Named "n" $ TInt] TInt) $ Block []
      $ Just $ Expr TInt
        $ EIf
          (Cond $ Expr TBln $ EApp
            (Expr (TFunc Pure [TInt, TInt] TBln) $ EIntr ILeq) Pure
            [Expr TInt $ EName "n", Expr TInt $ EVal $ VInt 1])
          (Expr TInt $ EVal $ VInt 1)
          (Expr TInt $ EApp
            (Expr (TFunc Pure [TInt, TInt] TInt) $ EIntr IMul) Pure
            [Expr TInt $ EName "n"
            , Expr TInt $ EApp
              (Expr (TFunc Pure [TInt] TInt) $ EName "fact") Pure
                [Expr TInt $ EApp (Expr (TFunc Pure [TInt, TInt] TInt) $ EIntr ISub)
                  Pure [Expr TInt $ EName "n", Expr TInt $ EVal $ VInt 1]
                ]
            ]
          )
     )
   ]
   []

  , testGroup "implicit conversions"
    [ test "Flt a = 5"
      [("a", UVar $ Var TFlt $ Expr TInt $ EVal $ VInt 5)]
      []

    , test "$ a = 5; Flt b = a"
      [ ("a", UVar $ Var TInt $ Expr TInt $ EVal $ VInt 5)
      , ("b", UVar $ Var TFlt $ Expr TInt $ EName "a")
      ]
      []

    , test "$ a = 5; $ b = 4.5; $ c = a + b"
      [ ("a", UVar $ Var TInt $ Expr TInt $ EVal $ VInt 5)
      , ("b", UVar $ Var TFlt $ Expr TFlt $ EVal $ VFlt 4.5)
      , ("c", UVar $ Var TFlt $ Expr TFlt $ EApp
          (Expr (TFunc Pure [TFlt, TFlt] TFlt) $ EIntr FAdd)
          Pure [Expr TInt $ EName "a", Expr TFlt $ EName "b"])
      ]
      []

    -- Thought to self:
    -- ^A <: ~A -> implicit dereference
    -- A <: ^A -> implicit reference
    -- ~^A <: ~^A -> reference assignment

    , namedTest "mutableInc" [s|
mutableInc(^~Int x):
    x = x + 1

inc(~Int x) =>
    mutableInc(x)
    x
|]
      [ ("mutableInc", UFunc $ Func
          (Sig Pure [Named "x" $ TRef $ TMut TInt] TNone) $ Block
          [SAssign (Expr (TRef $ TMut TInt) $ EName "x") $ Expr TInt
            $ EApp (Expr (TFunc Pure [TInt, TInt] TInt) $ EIntr IAdd) Pure
              [Expr (TRef $ TMut TInt) $ EName "x", Expr TInt $ EVal $ VInt 1]]
          Nothing)

      , ("inc", UFunc $ Func
          (Sig Pure [Named "x" $ TMut TInt] TInt ) $ Block
          [SExpr $ Expr TNone $ EApp
           (Expr (TFunc Pure [TRef $ TMut TInt] TNone) $ EName "mutableInc")
           Pure [Expr (TMut TInt) $ EName "x"]
          ]
          (Just $ Expr (TMut TInt) $ EName "x" ))
      ]
      []
    ]

  , testGroup "arrays"
  [ namedTest "array-cons-int"
    "$ arr = Array(2, 0)"
    [ ("arr", UVar $ Var (TArray TInt)
      $ Expr (TArray TInt) $ EApp
        (Expr (TFunc Pure [TInt, TInt] $ TArray TInt) $ EIntr ArrayCons) Pure
        [Expr TInt $ EVal $ VInt 2, Expr TInt $ EVal $ VInt 0])
    ]
    []

  , namedTest "array-cons-bln"
    "$ arr = Array(2, true)"
    [ ("arr", UVar $ Var (TArray TBln)
      $ Expr (TArray TBln) $ EApp
        (Expr (TFunc Pure [TInt, TBln] $ TArray TBln) $ EIntr ArrayCons) Pure
        [Expr TInt $ EVal $ VInt 2, Expr TBln $ EVal $ VBln True])
    ]
    []

  , namedTest "array-cons-bln-and-str"
    "$ as = Array(2, true); $ bs = Array(2, \"\")"
    [ ("as", UVar $ Var (TArray TBln)
      $ Expr (TArray TBln) $ EApp
        (Expr (TFunc Pure [TInt, TBln] $ TArray TBln) $ EIntr ArrayCons) Pure
        [Expr TInt $ EVal $ VInt 2, Expr TBln $ EVal $ VBln True])

    , ("bs", UVar $ Var (TArray TStr)
      $ Expr (TArray TStr) $ EApp
        (Expr (TFunc Pure [TInt, TStr] $ TArray TStr) $ EIntr ArrayCons) Pure
        [Expr TInt $ EVal $ VInt 2, Expr TStr $ EVal $ VStr ""])
    ]
    []

  , namedTest "mut-array-cons-bln"
    "~$ arr = Array(2, true)"
    [ ("arr", UVar $ Var (TMut $ TArray TBln)
      $ Expr (TArray TBln) $ EApp
        (Expr (TFunc Pure [TInt, TBln] $ TArray TBln) $ EIntr ArrayCons) Pure
        [Expr TInt $ EVal $ VInt 2, Expr TBln $ EVal $ VBln True])
    ]
    []

  , namedTest "array-app-desugared"
    "$ arr = Array(2, true); $ a = apply(arr, 0)"
    [ ("arr", UVar $ Var (TArray TBln)
      $ Expr (TArray TBln) $ EApp
        (Expr (TFunc Pure [TInt, TBln] $ TArray TBln) $ EIntr ArrayCons) Pure
        [Expr TInt $ EVal $ VInt 2, Expr TBln $ EVal $ VBln True])
    , ("a", UVar $ Var (TRef TBln)
        $ Expr (TRef TBln) $ EApp
          (Expr (TFunc Pure [TRef $ TArray TBln, TInt] $ TRef TBln) $ EIntr ArrayAppImt) Pure
          [Expr (TArray TBln) $ EName "arr", Expr TInt $ EVal $ VInt 0])
    ]
    []

  , namedTest "array-app-syntax-sugar-1"
    "$ arr = Array(2, true); $ a = arr.apply(0)"
    [ ("arr", UVar $ Var (TArray TBln)
      $ Expr (TArray TBln) $ EApp
        (Expr (TFunc Pure [TInt, TBln] $ TArray TBln) $ EIntr ArrayCons) Pure
        [Expr TInt $ EVal $ VInt 2, Expr TBln $ EVal $ VBln True])
    , ("a", UVar $ Var (TRef TBln)
        $ Expr (TRef TBln) $ EApp
          (Expr (TFunc Pure [TRef $ TArray TBln, TInt] $ TRef TBln) $ EIntr ArrayAppImt) Pure
          [Expr (TArray TBln) $ EName "arr", Expr TInt $ EVal $ VInt 0])
    ]
    []

  , namedTest "array-app-syntax-sugar-2"
    "$ arr = Array(2, true); $ a = arr(0)"
    [ ("arr", UVar $ Var (TArray TBln)
      $ Expr (TArray TBln) $ EApp
        (Expr (TFunc Pure [TInt, TBln] $ TArray TBln) $ EIntr ArrayCons) Pure
        [Expr TInt $ EVal $ VInt 2, Expr TBln $ EVal $ VBln True])
    , ("a", UVar $ Var (TRef TBln)
        $ Expr (TRef TBln) $ EApp
          (Expr (TFunc Pure [TRef $ TArray TBln, TInt] $ TRef TBln) $ EIntr ArrayAppImt) Pure
          [Expr (TArray TBln) $ EName "arr", Expr TInt $ EVal $ VInt 0])
    ]
    []

    , namedTest "array-update-desugared" [s|
f() -> Bln =>
    ~$ arr = Array(2, false)
    update(arr, 0, true)
    apply(arr, 0)
|]
      [ ("f", UFunc $ Func (Sig Pure [] TBln) $ Block
          [ SVar $ Named "arr" $ Var (TMut $ TArray TBln)
            $ Expr (TArray TBln) $ EApp
              (Expr (TFunc Pure [TInt, TBln] $ TArray TBln) $ EIntr ArrayCons) Pure
              [Expr TInt $ EVal $ VInt 2, Expr TBln $ EVal $ VBln False]
          , SExpr
            (Expr TNone $ EApp
              (Expr (TFunc Pure [TRef $ TMut $ TArray TBln, TInt, TRef TBln] TNone) $ EIntr ArrayUpdate)
              Pure
              [ Expr (TMut $ TArray TBln) $ EName "arr"
              , Expr TInt $ EVal $ VInt 0
              , Expr TBln $ EVal $ VBln True])
          ]
          $ Just $ Expr (TRef $ TMut TBln)
            $ EApp (Expr (TMut $ TArray TBln) $ EName "arr") Pure
              [Expr TInt $ EVal $ VInt 0]
        )
      ]
      []

    , namedTest "array-update-syntax-sugar" [s|
f() -> Bln =>
    ~$ arr = Array(2, false)
    arr(0) = true
    arr(0)
|]
      [ ("f", UFunc $ Func (Sig Pure [] TBln) $ Block
          [ SVar $ Named "arr" $ Var (TMut $ TArray TInt)
            $ Expr (TMut $ TArray TInt) $ EApp
              (Expr (TFunc Pure [TInt, TInt] $ TArray TInt) $ EName "Array") Pure
              [Expr TInt $ EVal $ VInt 2, Expr TBln $ EVal $ VBln False]
          , SAssign
            (Expr
              (TRef $ TMut TInt) $ EApp (Expr (TMut $ TArray TInt) $ EName "arr")
              Pure [Expr TInt $ EVal $ VInt 0])
            $ Expr TBln $ EVal $ VBln True
          ]
          $ Just $ Expr (TRef $ TMut TBln)
            $ EApp (Expr (TMut $ TArray TBln) $ EName "arr") Pure
              [Expr TInt $ EVal $ VInt 0]
        )
      ]
      []
    ]
  ]

