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

type LocalBindings = MultiMap Name Kind -- Really I might want to track values as well

data Bindings
  = GlobalBindings AstMc
  | LocalBindings LocalBindings Bindings

lookupKinds :: Bindings -> Name -> [Kind]
lookupKinds context name = case context of
  GlobalBindings ast -> astLookup ast name
  LocalBindings inner outer -> blockLookup name inner ++ lookupKinds outer name
  where
    astLookup :: AstMc -> Name -> [Kind]
    astLookup table n  = map kindOfUnit (multiLookup n table)

    kindOfUnit :: UnitMc -> Kind
    kindOfUnit u = case u of
      UNamespaceM _ -> KNamespace
      UClass _ -> KType

      -- TODO: how to handle this error case?
      UFuncM (Lambda (SigC purity params typeOrError) _ _) -> case typeOrError of
        Type returnType -> KExpr (TFunc purity (paramTypesOf params) returnType)

      UVar (VarMc _ typeOrError _) -> case typeOrError of
        Type t -> KExpr t
        Errors e -> KError e

    blockLookup :: Name -> LocalBindings -> [Kind]
    blockLookup n table = multiLookup n table

initBlockBindings :: Bindings -> Params -> Bindings
initBlockBindings outerContext params = LocalBindings (initLocalBindingsFromParams params) outerContext
  where
    initLocalBindingsFromParams :: Params -> LocalBindings
    initLocalBindingsFromParams = multiFromList . map paramToNameAndKind

    paramToNameAndKind :: Param -> (Name, Kind)
    -- TODO: This raises a really good question, what am I doing about mutability?
    paramToNameAndKind (Param m t n) = (n, KExpr t)

addLocalBinding :: Name -> Kind -> Bindings -> Bindings
addLocalBinding name kind (LocalBindings innerBindings outerBindings) =
  LocalBindings (multiInsert name kind innerBindings) outerBindings

