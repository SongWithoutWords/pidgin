{-# language DeriveDataTypeable #-}

import Data.Proxy

import Test.Tasty
import Test.Tasty.Options

import Ediff
import qualified Test.Lexer as Lexer
import qualified Test.Parser as Parser
import qualified Test.TypeCheck as TypeCheck
import qualified Test.CodeGen as CodeGen
import qualified Test.X.Reduce as Reduce

testTimeout_μs = 500000

main :: IO ()
main = defaultMainWithIngredients ingredients tests
  where
    ingredients
      = includingOptions [Option (Proxy :: Proxy Ediff)]
      : defaultIngredients

tests :: TestTree
tests = localOption (mkTimeout testTimeout_μs) $ testGroup "tests"
    [ Lexer.tests
    , Parser.tests
    , TypeCheck.tests
    , CodeGen.tests
    , Reduce.tests
    ]

