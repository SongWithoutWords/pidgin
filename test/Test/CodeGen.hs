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
fact(Int i) -> Int => if i <= 1 then 1 else i * fact(i - 1)
main() => fact(5)
|] 120

  , namedTest "fibonacci 7" [s|
fib(Int i) => if i <= 1 then 1 else fib(i - 1) + fib(i - 2)
main() => fib(7)
|] 21

  , namedTest "faster-fibonacci-7" [s|
fib(Int n) => fibImp(0, n, 1, 1)

fibImp(Int i, Int n, Int cur, Int next) -> Int =>
    cur if i >= n else fibImp(i + 1, n, next, cur + next)

main() => fib(7)
|] 21

  , namedTest "gcd 18 24" [s|
gcd(Int a, Int b) => a if b == 0 else gcd(b, a % b)
main() => gcd(18, 24)
|] 6

  , namedTest "seq" [s|
seq[A, B](A a, B b) => b
main() => seq(true, 7)
|] 7


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

  , testGroup "mathy"
    [ namedTest "sum-of-multiples-of-3-and-5" [s|
sumOfMultiples(Int n) =>
    0 if n <= 0 else sumOfMultiples(n - 1) + n if (n % 3 == 0 or n % 5 == 0) else 0

main() => sumOfMultiples(9)
|] 23

    , namedTest "even-fibonacci-sum" [s|
evenFibSum(Int max) => imp(max, 1, 1, 0)

imp(Int max, Int cur, Int next, Int sum) =>
    sum if cur > max else imp(max, next, cur + next, sum + cur if cur % 2 == 0 else 0)

main() =>
    evenFibSum(100)
|] 44

    , namedTest "largest-prime-factor" [s|
maxPrimeFactor(Int n, Int div, Int largestDiv) -> Int =>
    largestDiv if div > n else (maxPrimeFactor(n / div, div, div) if n % div == 0 else maxPrimeFactor(n, div + 1, largestDiv))

main() => maxPrimeFactor(13195, 2, 1)
|] 29

    , namedTest "palindrome-product" [s|
indexOfMaxDigit(Int n) => 0 if n < 10 else 1 + indexOfMaxDigit(n / 10)
tenToPower(Int n) => 1 if n <= 0 else 10 * tenToPower(n - 1)

isPalindrome(Int n) => isPalindromeImp(n, indexOfMaxDigit(n))
isPalindromeImp(Int n, Int maxIndex) =>
    $ maxIndexVal = tenToPower(maxIndex)
    $ leftDigit = n / maxIndexVal
    $ rightDigit = n % 10
    true if maxIndex <= 0 else
    (leftDigit == rightDigit) and isPalindromeImp((n % maxIndexVal) / 10, maxIndex - 2)

maxPalindrome(Int i) => maxPalindromeImp(i, i, 0)
maxPalindromeImp(Int i, Int j, Int max) -> Int =>
    $ ij = i * j
    max if i * i <= max else
    maxPalindromeImp(i - 1, i - 1, max) if ij <= max else
    maxPalindromeImp(i, j - 1, ij if ij > max else max) if isPalindrome(ij) else
    maxPalindromeImp(i, j - 1, max)

main() => maxPalindrome(99)
|] 9009

    , namedTest "sum-square-difference" [s|
sumOfSquares(Int n) => 1 if n <= 1 else n * n + sumOfSquares(n - 1)

sumTo(Int n) => 1 if n <= 1 else n + sumTo(n - 1)
squareOfSum(Int n) =>
    $ sum = sumTo(n)
    sum * sum

sumSquareDifference(Int n) => squareOfSum(n) - sumOfSquares(n)

main() => sumSquareDifference(10)
|] 2640

    , namedTest "nth-prime" [s|
prime(Int n) =>
    ~$ primes = Array(n, 0)
    update(arr, 0, 2)
    update(arr, 1, 3)
    primeImp(n, primes, 2, 3)

primeImp(Int n, ^~Array[Int] primes, Int numPrimesFound, Int candidate) =>
    if isPrime(candidate, primes, numPrimesFound):
        update(primes, numPrimesFound, candidate)
        primeImp(n, primes, numPrimesFound + 1, candidate + 2)
    else
        primeImp(n, primes, numPrimesFound, candidate + 2)

isPrime(candidate, primes, numRemainingPrimes) =>
    true if numRemainingPrimes <= 0

main() => prime(7)
|] 17
    ]
  ]

