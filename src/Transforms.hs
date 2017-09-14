module Transforms
  ( lexParseCheck
  , lexParseCheckGen
  , pidginToLlvmIr
  ) where

import System.IO.Unsafe

import LLVM.AST as A

import Ast
-- import MultiMapAst

import Lexer
import Parser
import PostParseAst
import TypeCheck
import CodeGen
import LlvmUtil

(|>) :: (a -> b) -> (b -> c) -> (a -> c)
f |> g = g . f

lexParseCheck :: String -> (Ast2, Errors)
lexParseCheck = scanTokens |> parse |> (fst . postParseAst) |> typeCheckAst

lexParseCheckGen :: String -> A.Module
lexParseCheckGen = (fst . lexParseCheck) |> codeGen

pidginToLlvmIr :: String -> IO String
pidginToLlvmIr src = llvmAstToAsm $ lexParseCheckGen src

