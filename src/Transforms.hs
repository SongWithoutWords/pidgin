module Transforms
  ( module Lexer
  , module Parser
  , module Transforms
  ) where

import System.IO.Unsafe

import LLVM.AST as A

import Ast
import Source

import Lexer
import Parser
import PostParseAst
import TypeCheck
import CodeGen
import LlvmUtil



-- TODO: Would be really great to wrap all of these transforms in a nice error monad

parseTreeFromSource :: SourceCode -> Ast0
parseTreeFromSource = parse . scanTokens

astFromSource :: SourceCode -> Ast1
astFromSource = fst . postParseAst . parseTreeFromSource

astFromParseTree :: Ast0 -> Ast1
astFromParseTree = fst . postParseAst

lexParseCheck :: SourceCode -> (Ast2, Errors)
lexParseCheck = typeCheckAst . astFromSource

typedAstFromSource :: SourceCode -> Ast2
typedAstFromSource = fst . lexParseCheck

typeErrorsFromSource :: SourceCode -> Errors
typeErrorsFromSource = snd . lexParseCheck

typedAstFromParseTree :: Ast0 -> Ast2
typedAstFromParseTree = fst . typeCheckAst . astFromParseTree

typeErrorsFromParseTree :: Ast0 -> Errors
typeErrorsFromParseTree = snd . typeCheckAst . astFromParseTree

llvmModuleFromSource :: SourceCode -> A.Module
llvmModuleFromSource = codeGen . typedAstFromSource

llvmIrFromSource :: SourceCode -> IO String
llvmIrFromSource = llvmAstToAsm . llvmModuleFromSource

returnValFromSource :: SourceCode -> IO (Maybe Int)
returnValFromSource = execMainOfLlvmAst . llvmModuleFromSource

