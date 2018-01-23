{-# language QuasiQuotes #-}
module Test.CodeGen(tests) where

import Control.Monad(unless)
import Data.String.QQ

import Test.Tasty
import Test.Tasty.HUnit

import Transforms

test :: String -> Int -> TestTree
test src = namedTest src src

namedTest :: String -> String -> Int -> TestTree
namedTest name src retValExpected = testCase name $ do
  let (ast, errors) = lexParseCheck src
  if not $ null errors
    then assertFailure $ "errors: " ++ show errors
    else do
      retValActual <- execMainOfLlvmAst $ codeGen ast
      unless (retValActual == Just retValExpected) $ do
        llvmIr <- llvmIrFromSource src
        assertFailure $ "expected:\n" ++ show retValExpected
          ++ "\nbut got:\n" ++ show retValActual
          ++ "\nwith llvm-ir:\n" ++ llvmIr

tests :: TestTree
tests = testGroup "codegen"
  [ namedTest "negative value" "main() => -1" (-1)

  , namedTest "double 8" [s|
double(Int i) => 2 * i
main() => double(8)
|] 16

  , namedTest "inc 7" [s|
inc(Int i) => i + 1
main() => inc(7)
|] 8

  , namedTest "inc 7 (local var)" [s|
inc(Int i) =>
    $ one = 1
    i + one
main() => inc(7)
|] 8

  , namedTest "square 6" [s|
sqr(Int i) => i * i
main() => sqr(6)
|] 36

  , namedTest "sum to 5" [s|
sumTo(Int i) => i * (i + 1) / 2
main() => sumTo(5)
|] 15

  , namedTest "factorial 4" [s|
fact(Int i) -> Int => 1 if i <= 1 else i * fact(i - 1)
main() => fact(5)
|] 120

  , namedTest "fibonacci 7" [s|
fib(Int i) => 1 if i <= 1 else fib(i - 1) + fib(i - 2)
main() => fib(7)
|] 21

  , namedTest "gcd 18 24" [s|
gcd(Int a, Int b) => a if b == 0 else gcd(b, a % b)
main() => gcd(18, 24)
|] 6

  , namedTest "7th-prime" [s|
nthPrime(Int n) =>
    ~$ arr = Array(n, 0)
    update(arr, 0, 2)
    update(arr, 1, 3)
    nthPrimImp(n, 2, 3)

nthPrimeImp(Int n, Int numPrimesFound, Int candidate, ^~Array[Int] primes) =>
    if isPrime(candidate, primes, numPrimesFound):
        update(primes, numPrimesFound, candidate)
        nthPrimeImp(n, numPrimesFound + 1, candidate + 2)
    else
        nthPrimeImp(n, numPrimesFound, candidate + 2)

main() => nthPrime(7)
|] 17

  -- Modulus operator tests
  , test "main() => 0 % 2" 0

  , test "main() => 5 % 5" 0

  , test "main() => 23 % 7" 2

  , test "main() => -3 % 7" 4

  -- Comparison operator tests

    -- Greater
  , test "main() => 2 > 1" 1

  , test "main() => 1 > 2" 0

  , test "main() => 1 > -2" 1

  , test "main() => -2 > 1" 0

    -- Lesser
  , test "main() => 1 < 2" 1

  , test "main() => 2 < 1" 0

  , test "main() => -2 < 1" 1

  , test "main() => 1 < -2" 0

    -- Greater-equal
  , test "main() => 2 >= 1" 1

  , test "main() => 1 >= 2" 0

  , test "main() => 1 >= -2" 1

  , test "main() => -2 >= 1" 0

  , test "main() => 1 >= 1" 1

  , test "main() => -2 >= -2" 1

    -- Lesser
  , test "main() => 1 <= 2" 1

  , test "main() => 2 <= 1" 0

  , test "main() => -2 <= 1" 1

  , test "main() => 1 <= -2" 0

  , test "main() => 1 <= 1" 1

  , test "main() => -2 <= -2" 1

    -- Equal
  , test "main() => 0 == 0" 1

  , test "main() => 3 == 4" 0

  , test "main() => 0 != 0" 0

  , test "main() => 3 != 4" 1

    -- And
  , test "main() => false and false" 0
  , test "main() => false and true" 0
  , test "main() => true and false" 0
  , test "main() => true and true" 1

    -- Or
  , test "main() => false or false" 0
  , test "main() => false or true" 1
  , test "main() => true or false" 1
  , test "main() => true or true" 1

  -- If expressions
  , test "main() => 3 if 1 < 2 else 7" 3
  , test "main() => 6 if false else -1" (-1)

  -- Combined-ops
  , test "main() => 0 == 1 or 1 == 1" 1
  , test "main() => 0 == 1 and 1 == 1" 0
  , test "main() => 7 % 5 == 0" 0
  , test "main() => 10 % 5 == 0" 1
  , test "main() => 4 if 4 % 2 == 0 else 0" 4
  , test "main() => 1 if 1 < 1 else 2 if 1 < 2 else 3" 2
  , test "main() => 1 if 1 < 1 else (2 if 1 < 2 else 3)" 2

  , testGroup "arrays"
    [ namedTest "array-update-desugared" [s|
main() -> Int =>
    ~$ arr = Array(1, 0)
    update(arr, 0, 7)
    apply(arr, 0)
|] 7
    , namedTest "array-element-add" [s|
main() -> Int =>
    ~$ arr = Array(2, 0)
    update(arr, 0, 7)
    update(arr, 1, 6)
    apply(arr, 0) + apply(arr, 1)
|] 13
    , namedTest "array-indexed-by-elements" [s|
main() -> Int =>
    ~$ arr = Array(4, 0)
    update(arr, 0, 2)
    update(arr, 1, 3)
    update(arr, 2, 4)
    update(arr, 3, 5)
    apply(arr, apply(arr, 0)) + apply(arr, apply(arr, 1))
|] 9
    , namedTest "array-sum" [s|
arraySum(^Array[Int] array, Int size) =>
    (apply(array, size - 1) + arraySum(array, size - 1)) if size > 0 else 0

main() =>
    ~$ arr = Array(4, 0)
    update(arr, 0, 4)
    update(arr, 1, 5)
    update(arr, 2, 7)
    update(arr, 3, 9)
    arraySum(arr, 4)
|] 25
    ]
  ]

