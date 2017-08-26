{-# language GADTs #-}

module TypeContext
  ( module Ast
  , module TypeContext
  , module Kinds
  ) where

import Ast
import AstUtil
import Kinds

import MultiMap


data Bindings
  = GlobalBindings AstMc
  | BlockBindings {innerScope::AstMc, outerScope::Bindings}

lookupKinds :: Bindings -> Name -> [Kind]
lookupKinds context name = case context of
  GlobalBindings table -> lookupInAst table name
  BlockBindings inner outer -> lookupInAst inner name ++ lookupKinds outer name
  where
    lookupInAst :: Map Name [UnitMc] -> Name -> [Kind]
    lookupInAst table n  = map kindOfUnit (multiLookup n table)

    kindOfUnit :: UnitMc -> Kind
    kindOfUnit u = case u of
      UNamespaceM _ -> KNamespace
      UClass _ -> KType

      -- TODO: how to handle this error case?
      UFuncM (Lambda (SigC purity params typeOrError) _) -> case typeOrError of
        Type returnType -> KExpr (TFunc purity (paramTypesOf params) returnType)

      UVar (VarMc _ typeOrError _) -> case typeOrError of
        Type t -> KExpr t
        Errors e -> KError e

initBlockBindings :: Bindings -> Params -> Bindings
initBlockBindings outerContext params = BlockBindings (initLocalContextFromParams params) outerContext
  where
    initLocalContextFromParams :: Params -> Map Name [UnitMc]
    initLocalContextFromParams = multiFromList . map paramToUnit

    paramToUnit :: Param -> (String, UnitMc)
    paramToUnit (Param m t n) = (n, UVar $ VarMc m (Type t) $ EName n)

