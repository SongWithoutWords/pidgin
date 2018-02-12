module X.TypeCheck.Util where

import Data.Maybe(mapMaybe)

import X.Ast
import Util.MultiMap

typeOfExpr :: Expr -> Type
typeOfExpr (Expr t _) = t

typeOfFunc :: Func -> Type
typeOfFunc (Func (Sig purity params returnType) _) =
  TFunc purity (map (\(_, t) -> t) params) returnType

typeOfVar :: Var -> Type
typeOfVar (Var t _) = t

kindOfUnit :: Name -> Unit -> Kind
kindOfUnit name u = case u of
  UNamespace n -> KNamespace n
  UData ms -> KType $ TData name ms
  UFunc f -> KExpr $ Expr (typeOfFunc f) $ EBinding
  UVar (Var t _) -> KExpr $ Expr t $ EBinding

lookupUnit :: Name -> Units -> Kinds
lookupUnit n us = kindOfUnit n <$> multiLookup n us

-- consOfClass :: Class -> [Type]
-- consOfClass (Class members) = mapMaybe
--   (\m -> case m of
--       MCons _ f -> Just $ typeOfFunc f
--       _ -> Nothing
--   ) $ multiValues members


