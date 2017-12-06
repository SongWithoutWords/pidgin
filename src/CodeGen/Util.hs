module CodeGen.Util
  ( module CodeGen.Util
  , fromString
  ) where

import Data.String

import qualified LLVM.AST as A
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Type as T

import Ast
import CodeGen.CodeGenM

typeToLlvmType :: Type2 -> A.Type
typeToLlvmType t = case t of

  TFunc _ paramTypes ret -> T.FunctionType
    { T.resultType = typeToLlvmType ret
    , T.argumentTypes = map typeToLlvmType paramTypes
    , T.isVarArg = False
    }

  TBln -> T.i1
  TChr -> T.i8
  TFlt -> T.float
  TInt -> T.i64
  TNone -> T.void
  _ -> error $ "typeToLlvmType " ++ show t

-- nameToLlvmName :: Name -> A.Name
-- nameToLlvmName = fromString

localReference :: Name -> Type2 -> A.Operand
localReference name typ = A.LocalReference (typeToLlvmType typ) (fromString name)

globalReference :: Name -> Type2 -> A.Operand
globalReference name typ =
  A.ConstantOperand $ C.GlobalReference (T.ptr $ typeToLlvmType typ) (fromString name)

typeOfOperand :: A.Operand -> A.Type
typeOfOperand op = case op of
  A.LocalReference typ _ -> typ

addLocalBinding :: String -> Type2 -> CodeGenM ()
addLocalBinding name typ = addBinding name $ localReference name typ


