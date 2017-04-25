module TestComposer where

import TestCase

import Ast
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

typedAst :: Ast -> TestCase
typedAst a = mempty {testTypedAst = Just a}

typeErrors :: Errors -> TestCase
typeErrors e = mempty {testTypeErrors = Just e}

