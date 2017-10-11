{-# language GADTs #-}
module TypeCheck.Substitution where

import qualified Data.Map as M

import Ast.Type

type Substitutions = M.Map Word Type2

subType :: Substitutions -> Type2 -> Type2
subType s typ = let subType' = subType s in case typ of

  TFunc p paramTypes retType ->
    TFunc p (subType' <$> paramTypes) (subType' retType)

  TVar tvar -> case M.lookup tvar s of
    Nothing -> TVar tvar
    Just t -> t

  t -> t

