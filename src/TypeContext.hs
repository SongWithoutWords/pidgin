{-# language GADTs #-}

module TypeContext where

import Ast
import AstUtil
import TypeErrors
-- import qualified Ast1 as A1

import MultiMap

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
  = GlobalBindings (Map Name [UnitMc])
  | BlockBindings {innerScope:: Map Name [UnitMc], outerScope::Bindings}

lookupKinds :: Bindings -> Name -> [Kind]
lookupKinds context name = case context of
  GlobalBindings table -> lookupInAst table name
  BlockBindings local global -> lookupInAst local name ++ lookupKinds global name
  where
    lookupInAst :: Map Name [UnitMc] -> Name -> [Kind]
    lookupInAst table n  = map kindOfUnit (multiLookup n table)--case multiLookup n ast of

    kindOfUnit :: UnitMc -> Kind
    kindOfUnit u = case u of
      UNamespaceM _ -> KNamespace
      UClass _ -> KType
      UFuncM (Lambda (SigC _ _ typeOrError) _) -> case typeOrError of Type t -> KExpr t
      UVar (VarMc _ typeOrError _) -> case typeOrError of Type t -> KExpr t

initBlockBindings :: Bindings -> Params -> Bindings
initBlockBindings outerContext params = BlockBindings (initLocalContextFromParams params) outerContext
  where
    initLocalContextFromParams :: Params -> Map Name [UnitMc]
    initLocalContextFromParams = multiFromList . map paramToUnit

    paramToUnit :: Param -> (String, UnitMc)
    paramToUnit (Param m t n) = (n, UVar $ VarMc m (Type t) $ EName n)

