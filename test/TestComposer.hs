module TestComposer where

import TestCase

import Ast
import Ast.Error
import Lexer.Token
import Util.MultiMap

-- example usage: myTest = source "$ a = 5; Bln b = a"
--    <> typeErrors [TypeConflict TBln TInt]

name :: String -> TestCase
name n = mempty {testName = Just n}

source :: String -> TestCase
source s = mempty {testSource = Just s}

tokens :: Tokens -> TestCase
tokens t = mempty {testTokens = Just t}

ast :: Ast0 -> TestCase
ast a = mempty {testAst = Just a}

typedAst :: [(Name, Unit2)] -> TestCase
typedAst pairs = mempty {testTypedAst = Just $ multiFromList pairs}

typeErrors :: Errors -> TestCase
typeErrors e = mempty {testTypeErrors = Just e}

returnVal :: Int -> TestCase
returnVal i = mempty {testReturnVal = Just i, testTypeErrors = Just []}

