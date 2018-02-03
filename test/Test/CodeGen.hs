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
    if i >= n then cur else fibImp(i + 1, n, next, cur + next)

main() => fib(7)
|] 21

  , namedTest "gcd 18 24" [s|
gcd(Int a, Int b) => if b == 0 then a else gcd(b, a % b)
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
  , test "main() => if 1 < 2 then 3 else 7" 3
  , test "main() => if false then 6 else -1" (-1)

  -- Combined-ops
  , test "main() => 0 == 1 or 1 == 1" 1
  , test "main() => 0 == 1 and 1 == 1" 0
  , test "main() => 7 % 5 == 0" 0
  , test "main() => 10 % 5 == 0" 1
  , test "main() => if 4 % 2 == 0 then 4 else 0" 4
  , test "main() => if 1 < 1 then 1 else if 1 < 2 then 2 else 3" 2
  , test "main() => if 1 < 1 then 1 else (if 1 < 2 then 2 else 3)" 2

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
    if size > 0 then (apply(array, size - 1) + arraySum(array, size - 1)) else 0

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
    if n <= 0 then 0 else sumOfMultiples(n - 1) + if (n % 3 == 0 or n % 5 == 0) then n else 0

main() => sumOfMultiples(9)
|] 23

    , namedTest "even-fibonacci-sum" [s|
evenFibSum(Int max) => imp(max, 1, 1, 0)

imp(Int max, Int cur, Int next, Int sum) =>
    if cur > max then sum else imp(max, next, cur + next, sum + if cur % 2 == 0 then cur else 0)

main() =>
    evenFibSum(100)
|] 44

    , namedTest "largest-prime-factor" [s|
maxPrimeFactor(Int n, Int div, Int largestDiv) -> Int =>
    if div > n then
        largestDiv
    else if n % div == 0 then
        maxPrimeFactor(n / div, div, div)
    else
        maxPrimeFactor(n, div + 1, largestDiv)

main() => maxPrimeFactor(13195, 2, 1)
|] 29

    , namedTest "palindrome-product" [s|
indexOfMaxDigit(Int n) => if n < 10 then 0 else 1 + indexOfMaxDigit(n / 10)
tenToPower(Int n) => if n <= 0 then 1 else 10 * tenToPower(n - 1)

isPalindrome(Int n) => isPalindromeImp(n, indexOfMaxDigit(n))
isPalindromeImp(Int n, Int maxIndex) =>
    $ maxIndexVal = tenToPower(maxIndex)
    $ leftDigit = n / maxIndexVal
    $ rightDigit = n % 10
    if maxIndex <= 0 then
        true
    else
        leftDigit == rightDigit and isPalindromeImp((n % maxIndexVal) / 10, maxIndex - 2)

maxPalindrome(Int i) => maxPalindromeImp(i, i, 0)
maxPalindromeImp(Int i, Int j, Int max) -> Int =>
    $ ij = i * j
    if i * i <= max then
        max
    else if ij <= max then
        maxPalindromeImp(i - 1, i - 1, max)
    else if isPalindrome(ij) then
        maxPalindromeImp(i, j - 1, if ij > max then ij else max)
    else
        maxPalindromeImp(i, j - 1, max)

main() => maxPalindrome(99)
|] 9009

    , namedTest "sum-square-difference" [s|
sumOfSquares(Int n) => if n <= 1 then 1 else n * n + sumOfSquares(n - 1)

sumTo(Int n) => if n <= 1 then 1 else n + sumTo(n - 1)
squareOfSum(Int n) =>
    $ sum = sumTo(n)
    sum * sum

sumSquareDifference(Int n) => squareOfSum(n) - sumOfSquares(n)

main() => sumSquareDifference(10)
|] 2640

    , namedTest "nth-prime" [s|
prime(Int n) -> Int =>
    ~$ primes = Array(n, 0)
    update(primes, 0, 2)
    update(primes, 1, 3)
    primeImp(n - 1, primes, 2, 3)

primeImp(Int n, ^~Array[Int] primes, Int numPrimesFound, Int candidate) =>
    if numPrimesFound > n then
        apply(primes, n)
    else if isPrime(candidate, primes, numPrimesFound) then
        update(primes, numPrimesFound, candidate)
        primeImp(n, primes, numPrimesFound + 1, candidate + 2)
    else
        primeImp(n, primes, numPrimesFound, candidate + 2)

isPrime(Int candidate, ^Array[Int] primes, Int numRemainingPrimes) =>
    if numRemainingPrimes <= 0 then
        true
    else if candidate % apply(primes, numRemainingPrimes - 1) == 0 then
        false
    else
        isPrime(candidate, primes, numRemainingPrimes - 1)

main() => prime(7)
|] 17

    , namedTest "longest-collatz-sequence" [s|

collatz(Int n) =>
    collatzImp(n, 1)

collatzImp(Int n, Int steps) =>
    if n == 1 then
        steps
    else if n % 2 == 0 then
        collatzImp(n / 2, steps + 1)
    else
        collatzImp(3 * n + 1, steps + 1)

longestCollatz(Int n) => longestCollatzImp(n, 0, 1)

longestCollatzImp(Int n, Int maxSteps, Int maxStepNumber) =>
    if n == 1 then
        maxStepNumber
    else
        $ curSteps = collatz(n)
        longestCollatzImp(n - 1, if curSteps > maxSteps then curSteps else maxSteps, if curSteps > maxSteps then n else maxStepNumber)

main() => longestCollatz(10)
|] 9

    , namedTest "coin-sums" [s|

coinSum(Int total) =>
    cs(total, 200)

cs(Int total, Int maxCoin) =>
    $ max = if total < maxCoin then total else maxCoin
    trySum(total, max, 200) + trySum(total, max, 100) + trySum(total, max, 50) + trySum(total, max, 20) + trySum(total, max, 10) + trySum(total, max, 5) + trySum(total, max, 2) + trySum(total, max, 1)

trySum(Int total, Int max, Int coin) =>
    if coin > max then 0 else if total == coin then 1 else cs(total - coin, coin)

main() => coinSum(10)
|] 11
    ]
  ]

