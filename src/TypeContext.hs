-- {-# language FlexibleInstances #-}

module TypeContext where

import qualified Data.Map.Lazy as Map

import Ast
import AstUtil
import qualified Ast1 as A1

data TypeContext = TypeContext
  { tcSearchHistory::[Name]
  , tcBindings::Bindings}

pushSearchName :: Name -> TypeContext -> TypeContext
pushSearchName name (TypeContext names bindings) = TypeContext (name:names) bindings

popSearchName :: TypeContext -> TypeContext
popSearchName tc@(TypeContext names bindings) = case names of
  _:ns -> TypeContext ns bindings
  _ -> tc


data Bindings
  = GlobalBindings A1.Ast
  | BlockBindings {innerScope::A1.Ast, outerScope::Bindings}

lookupKinds :: Bindings -> Name -> [Kind]
lookupKinds context name = case context of
  GlobalBindings ast -> lookupInAst ast name
  BlockBindings local global -> lookupInAst local name ++ lookupKinds global name
  where
    lookupInAst :: A1.Ast -> Name -> [Kind]
    lookupInAst ast n  = case Map.lookup n ast of
      Nothing -> []
      Just u -> case u of
        A1.UNamespace _ -> [KNamespace]
        A1.UClass _ -> [KType]
        A1.UFunc (Lambda s _) -> [KExpr $ Just $ typeOf s]
        A1.UVar (A1.Var _ t _) -> [KExpr t]

initBlockBindings :: Bindings -> Params -> Bindings
initBlockBindings outerContext params = BlockBindings (initLocalContextFromParams params) outerContext
  where
    initLocalContextFromParams :: Params -> A1.Ast
    initLocalContextFromParams = Map.fromList . map paramToUnit

    paramToUnit :: Param -> (String, A1.Unit)
    paramToUnit (Param m t n) = (n, A1.UVar $ A1.Var m (Just t) $ EName n)

