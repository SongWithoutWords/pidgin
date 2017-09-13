module CodeGenUtil where

import Data.String

import qualified LLVM.AST as A
import qualified LLVM.AST.Type as T

import Ast
import CodeGenM

typeToLlvmType :: Type2 -> A.Type
typeToLlvmType t = case t of
  TBln -> T.i1
  TChr -> T.i8
  TFlt -> T.float
  TInt -> T.i32

nameToLlvmName :: Name -> A.Name
nameToLlvmName = fromString

typeOfOperand :: A.Operand -> A.Type
typeOfOperand op = case op of
  A.LocalReference typ _ -> typ

addLocalBinding :: String -> Type2 -> CodeGenM ()
addLocalBinding name typ = addBinding name
  $ A.LocalReference (typeToLlvmType typ) (nameToLlvmName name)


