module TestCase
  ( module TestCase
  , module Data.Monoid
  ) where

import Control.Applicative
import Data.Monoid

import Ast
import Ast.Error
import Tokens


type TestCases = [TestCase]

data TestCase = TestCase
  { testName :: Maybe String
  , testSource :: Maybe String
  , testTokens :: Maybe Tokens
  , testAst :: Maybe Ast0
  , testTypedAst :: Maybe Ast2
  , testTypeErrors :: Maybe Errors
  , testReturnVal :: Maybe Int}
  deriving(Eq, Show)

instance Monoid TestCase where
  mempty = TestCase
    mempty
    Nothing
    mempty
    mempty
    mempty
    mempty
    Nothing
  mappend a b = TestCase
    (testName a <> testName b)
    (testSource a <|> testSource b)
    (testTokens a <> testTokens b)
    (testAst a <> testAst b)
    (testTypedAst a <> testTypedAst b)
    (testTypeErrors a <> testTypeErrors b)
    (testReturnVal a <|> testReturnVal b)

