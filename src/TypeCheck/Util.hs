module TypeCheck.Util where

import Data.Maybe(mapMaybe)

import Ast.A2Constrained
import Util.MultiMap

typeOfExpr :: Expr -> Type
typeOfExpr (Expr t _) = t

typeOfFunc :: Func -> Type
typeOfFunc (Func (Sig purity params returnType) _) =
  TFunc purity (map (\(_, t) -> t) params) returnType

typeOfVar :: Var -> Type
typeOfVar (Var t _) = t

-- consOfClass :: Class -> [Type]
-- consOfClass (Class members) = mapMaybe
--   (\m -> case m of
--       MCons _ f -> Just $ typeOfFunc f
--       _ -> Nothing
--   ) $ multiValues members


