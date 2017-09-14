{-# language GADTs #-}

module TypeContext
  ( module Ast
  , module TypeContext
  ) where

import Ast

import MultiMap

type LocalBindings = MultiMap Name Kind -- Really I might want to track values as well

data Bindings
  = GlobalBindings Ast2
  | LocalBindings LocalBindings Bindings

lookupKinds :: Bindings -> Name -> [Kind]
lookupKinds context name = case context of
  GlobalBindings ast -> astLookup ast name
  LocalBindings inner outer -> blockLookup name inner ++ lookupKinds outer name
  where
    astLookup :: Ast2 -> Name -> [Kind]
    astLookup table n  = map kindOfUnit (multiLookup n table)

    kindOfUnit :: Unit2 -> Kind
    kindOfUnit u = case u of
      UNamespace1 _ -> KNamespace
      UClass _ -> KType

      UFunc (Func1 (Sig2 purity params returnType) _) ->
        KExpr (TFunc purity (map (\(Param _ t _) -> t) params) returnType)

      UVar (Var2 _ typ _) -> KExpr typ

    blockLookup :: Name -> LocalBindings -> [Kind]
    blockLookup n table = multiLookup n table

initBlockBindings :: Bindings -> Params2 -> Bindings
initBlockBindings outerContext params =
  LocalBindings (initLocalBindingsFromParams params) outerContext
  where
    initLocalBindingsFromParams :: Params2 -> LocalBindings
    initLocalBindingsFromParams = multiFromList . map paramToNameAndKind

    paramToNameAndKind :: Param2 -> (Name, Kind)

    -- TODO: This raises a good question, how am I enforcing mutability?
    paramToNameAndKind (Param _ t n) = (n, KExpr t)

addLocalBinding :: Name -> Kind -> Bindings -> Bindings
addLocalBinding name kind (LocalBindings innerBindings outerBindings) =
  LocalBindings (multiInsert name kind innerBindings) outerBindings

