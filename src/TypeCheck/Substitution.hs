{-# language GADTs #-}
module TypeCheck.Substitution where

import qualified Data.Map as M

import Ast.Type

type Substitutions = M.Map Word Type2

subType :: Substitutions -> Type2 -> Type2
subType s typ = subType' typ
  where
    subType' :: Type2 -> Type2

    subType' (TFunc p paramTypes retType) =
      TFunc p (subType' <$> paramTypes) (subType' retType)

    subType' TBln = TBln
    subType' TInt = TInt
    subType' (TError e) = TError e
    subType' (TVar tvar) = case M.lookup tvar s of
      Nothing -> TVar tvar
      Just t -> t
