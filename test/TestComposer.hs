module TestComposer where

import qualified Data.Map.Lazy as Map

import TestCase

import Ast
import qualified Ast1 as A1
import Tokens
import TypeErrors

-- example usage: myTest = source "$ a = 5; Bln b = a" <> typeErrors [TypeConflict TBln TInt]

name :: String -> TestCase
name n = mempty {testName = Just n}

source :: String -> TestCase
source s = mempty {testSource = Just s}

tokens :: Tokens -> TestCase
tokens t = mempty {testTokens = Just t}

ast :: Ast -> TestCase
ast a = mempty {testAst = Just a}

typedAst :: [(String, A1.Unit)] -> TestCase
typedAst pairs = mempty {testTypedAst = Just $ Map.fromList pairs}

typeErrors :: Errors -> TestCase
typeErrors e = mempty {testTypeErrors = Just e}

