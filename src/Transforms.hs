module Transforms
  ( module Lexer
  , module Parser
  , module Transforms
  ) where

import System.IO.Unsafe

import LLVM.AST as A

import qualified Ast.A0Parse as A0
import qualified Ast.A1PostParse as A1
import Ast.A2Constrained.Error
import qualified Ast.A3Typed as A3

import Lexer
import Parser
import PostParse
import TypeCheck
import TypeCheck.ConstraintGen
import CodeGen
import LlvmUtil


-- TODO: Would be really great to wrap all of these transforms in a nice error monad

parseTreeFromSource :: String -> A0.Ast
parseTreeFromSource = parse . scanTokens

astFromSource :: String -> A1.Ast
astFromSource = fst . postParse . parseTreeFromSource

astFromParseTree :: A0.Ast -> A1.Ast
astFromParseTree = fst . postParse

constraintsFromSource :: String -> [Constraint]
constraintsFromSource src =
  let (_, cons, _) = (constrainAst $ astFromSource src) in cons

lexParseCheck :: String -> (A3.Ast, Errors)
lexParseCheck = typeCheckAst . astFromSource

typedAstFromSource :: String -> A3.Ast
typedAstFromSource = fst . lexParseCheck

typeErrorsFromSource :: String -> Errors
typeErrorsFromSource = snd . lexParseCheck

typedAstFromParseTree :: A0.Ast -> A3.Ast
typedAstFromParseTree = fst . typeCheckAst . astFromParseTree

typeErrorsFromParseTree :: A0.Ast -> Errors
typeErrorsFromParseTree = snd . typeCheckAst . astFromParseTree

llvmModuleFromSource :: String -> A.Module
llvmModuleFromSource = codeGen . typedAstFromSource

llvmIrFromSource :: String -> IO String
llvmIrFromSource = llvmAstToAsm . llvmModuleFromSource

returnValFromSource :: String -> IO (Maybe Int)
returnValFromSource = execMainOfLlvmAst . llvmModuleFromSource

