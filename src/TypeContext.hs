{-# language GADTs #-}

module TypeContext
  ( module Ast
  , module TypeContext
  ) where

import Ast

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

      UFuncM (Lambda (SigC purity params returnType) _ _) ->
        KExpr (TFunc purity (map (\(Param _ t _) -> t) params) returnType)

      UVar (VarMc _ typ _) -> KExpr typ

    blockLookup :: Name -> LocalBindings -> [Kind]
    blockLookup n table = multiLookup n table

initBlockBindings :: Bindings -> ParamsT -> Bindings
initBlockBindings outerContext params = LocalBindings (initLocalBindingsFromParams params) outerContext
  where
    initLocalBindingsFromParams :: ParamsT -> LocalBindings
    initLocalBindingsFromParams = multiFromList . map paramToNameAndKind

    paramToNameAndKind :: ParamT -> (Name, Kind)

    -- TODO: This raises a good question, how am I enforcing mutability?
    paramToNameAndKind (Param _ t n) = (n, KExpr t)

addLocalBinding :: Name -> Kind -> Bindings -> Bindings
addLocalBinding name kind (LocalBindings innerBindings outerBindings) =
  LocalBindings (multiInsert name kind innerBindings) outerBindings

