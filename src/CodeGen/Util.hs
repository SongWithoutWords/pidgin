module CodeGen.Util
  ( module CodeGen.Util
  , fromString
  ) where

import Data.String

import qualified LLVM.AST as A
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Type as T

import Ast.A3Typed
import Ast.Common.Name
import CodeGen.CodeGenM

typeToLlvmType :: Type -> A.Type
typeToLlvmType typ = case typ of

  TFunc _ paramTypes ret -> T.FunctionType
    { T.resultType = typeToLlvmType ret
    , T.argumentTypes = map typeToLlvmType paramTypes
    , T.isVarArg = False
    }

  TRef (TMut (TArray t)) -> T.ptr (typeToLlvmType t)
  TRef (TArray t) -> T.ptr (typeToLlvmType t)

  TRef (TMut t) -> T.ptr (typeToLlvmType t)
  TRef t -> T.ptr (typeToLlvmType t)

  TBln -> T.i1
  TChr -> T.i8
  TFlt -> T.float
  TInt -> T.i64
  TNone -> T.void
  _ -> error $ "typeToLlvmType " ++ show typ

-- nameToLlvmName :: Name -> A.Name
-- nameToLlvmName = fromString

localReference :: Name -> Type -> A.Operand
localReference name typ = A.LocalReference (typeToLlvmType typ) (fromString name)

globalReference :: Name -> Type -> A.Operand
globalReference name typ =
  A.ConstantOperand $ C.GlobalReference (T.ptr $ typeToLlvmType typ) (fromString name)

addLocalBinding :: String -> Type -> CodeGenM ()
addLocalBinding name typ = addBinding name $ localReference name typ


