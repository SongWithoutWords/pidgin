module Transforms
  ( scanTokens
  , parse
  , lexParseCheck
  , lexParseCheckGen
  , translateToLlvmIr
  , evalMain
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

(|>) :: (a -> b) -> (b -> c) -> (a -> c)
f |> g = g . f

lexParseCheck :: SourceCode -> (Ast2, Errors)
lexParseCheck = scanTokens |> parse |> (fst . postParseAst) |> typeCheckAst

lexParseCheckGen :: SourceCode -> A.Module
lexParseCheckGen = (fst . lexParseCheck) |> codeGen

translateToLlvmIr :: SourceCode -> IO String
translateToLlvmIr = llvmAstToAsm . lexParseCheckGen

evalMain :: SourceCode -> IO (Maybe Int)
evalMain = execMainOfLlvmAst . lexParseCheckGen

