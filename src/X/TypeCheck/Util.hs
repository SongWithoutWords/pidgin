module X.TypeCheck.Util where

import Data.Maybe(mapMaybe)

import X.Ast
import Util.MultiMap

-- kindsOfExpr :: Expr -> Kinds
-- kindsOfExpr (Expr k _) = k

typeOfFunc :: Func -> Type
typeOfFunc (Func _ (Sig purity params returnType) _) =
  TFunc purity (map (\(_, t) -> t) params) returnType

exprOfVar :: Var -> Expr
exprOfVar (Var _ expr) = expr

-- kindOfUnit :: Name -> Unit -> Kind
-- kindOfUnit name u = case u of
--   UNamespace n -> KNamespace n
--   UType ms -> KType ms
--   UFunc f -> KExpr $ typeOfFunc f
--   UVar e -> kindsOfExpr e

-- idOfUnit :: Unit -> Id
-- idOfUnit u = case u of
--   UNamespace units -> INamespace units
--   UType tId -> IType tId
--   UVar vId -> IVar vId
--   UFunc 

-- lookupUnit :: Name -> Units -> Ids
-- lookupUnit n us = kindOfUnit n <$> multiLookup n us

-- consOfClass :: Class -> [Type]
-- consOfClass (Class members) = mapMaybe
--   (\m -> case m of
--       MCons _ f -> Just $ typeOfFunc f
--       _ -> Nothing
--   ) $ multiValues members


