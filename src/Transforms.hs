module Transforms
  ( module Lexer
  , module Parser
  , module Transforms
  ) where

import System.IO.Unsafe

import LLVM.AST as A

import Ast
import Ast.Error

import Lexer
import Parser
import PostParseAst
import TypeCheck
import CodeGen
import LlvmUtil



-- TODO: Would be really great to wrap all of these transforms in a nice error monad

parseTreeFromSource :: String -> Ast0
parseTreeFromSource = parse . scanTokens

astFromSource :: String -> Ast1
astFromSource = fst . postParseAst . parseTreeFromSource

astFromParseTree :: Ast0 -> Ast1
astFromParseTree = fst . postParseAst

lexParseCheck :: String -> (Ast2, Errors)
lexParseCheck = typeCheckAst . astFromSource

typedAstFromSource :: String -> Ast2
typedAstFromSource = fst . lexParseCheck

typeErrorsFromSource :: String -> Errors
typeErrorsFromSource = snd . lexParseCheck

typedAstFromParseTree :: Ast0 -> Ast2
typedAstFromParseTree = fst . typeCheckAst . astFromParseTree

typeErrorsFromParseTree :: Ast0 -> Errors
typeErrorsFromParseTree = snd . typeCheckAst . astFromParseTree

llvmModuleFromSource :: String -> A.Module
llvmModuleFromSource = codeGen . typedAstFromSource

llvmIrFromSource :: String -> IO String
llvmIrFromSource = llvmAstToAsm . llvmModuleFromSource

returnValFromSource :: String -> IO (Maybe Int)
returnValFromSource = execMainOfLlvmAst . llvmModuleFromSource

