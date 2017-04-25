module TestCase
  ( module TestCase
  , module Data.Monoid
  ) where

-- import Preface

import Ast
import Tokens
import TypeErrors


-- import Data.Generics.Maybe
-- import Data.Maybe
import Data.Monoid

type TestCases = [TestCase]

data TestCase = TestCase
  { testName :: Maybe String
  , testSource :: Maybe String
  , testTokens :: Maybe Tokens
  , testAst :: Maybe Ast
  , testTypedAst :: Maybe Ast
  , testTypeErrors :: Maybe Errors }
  deriving(Eq, Show)

instance Monoid TestCase where
  mempty = TestCase
    mempty
    mempty
    mempty
    mempty
    mempty
    mempty
  mappend a b = TestCase
    (testName a <> testName b)
    (testSource a <> testSource b)
    (testTokens a <> testTokens b)
    (testAst a <> testAst b)
    (testTypedAst a <> testTypedAst b)
    (testTypeErrors a <> testTypeErrors b)


