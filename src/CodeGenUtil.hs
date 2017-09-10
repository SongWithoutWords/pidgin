module CodeGenUtil where

import qualified LLVM.AST as LAst

import qualified LLVM.AST.Type as LType

import Ast

typeToLlvmType :: TypeT -> LAst.Type
typeToLlvmType t = case t of
  TBln -> LType.i1
  TChr -> LType.i8
  TFlt -> LType.float
  TInt -> LType.i32


