module CodeGenUtil where

import Data.String

import qualified LLVM.AST as A
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Type as T

import Ast
import CodeGenM
import Debug

typeToLlvmType :: Type2 -> A.Type
typeToLlvmType t = trace ("typeToLlvmType " ++ show t) $ case t of
  TBln -> T.i1
  TChr -> T.i8
  TFlt -> T.float
  TInt -> T.i64
  TNone -> T.void

nameToLlvmName :: Name -> A.Name
nameToLlvmName = fromString

localReference :: Name -> Type2 -> A.Operand
localReference name typ = A.LocalReference (typeToLlvmType typ) (nameToLlvmName name)

globalReference :: Name -> Type2 -> A.Operand
globalReference name typ =
  A.ConstantOperand $ C.GlobalReference (typeToLlvmType typ) (nameToLlvmName name)

typeOfOperand :: A.Operand -> A.Type
typeOfOperand op = case op of
  A.LocalReference typ _ -> typ

addLocalBinding :: String -> Type2 -> CodeGenM ()
addLocalBinding name typ = addBinding name $ localReference name typ


