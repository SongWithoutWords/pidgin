{-# language FlexibleInstances #-}
{-# language TypeSynonymInstances #-}

module TypeCheck(typeCheck, Result(..)) where

import Ast
import TypeErrors

-- TODO: I think this pattern may lend itself to a monad

data Result a = Result { typedResult :: a, errors :: TypeErrors } deriving(Eq, Show)

class Checked a where
  typeCheck :: a -> Result a

instance Checked Ast where
  typeCheck ast =
    let uResults = map typeCheck ast
    in Result (map typedResult uResults) (concatMap errors uResults)

instance Checked Unit where
  typeCheck unit = case unit of
    UNamespace name units ->
      let unitsChkd = map typeCheck units
      in Result (UNamespace name $ map typedResult unitsChkd) (concatMap errors unitsChkd)
    c@UClass {} -> Result c [] -- TODO
    f@UFunc {} -> Result f [] -- TODO
    UVar v ->
      let vRes = typeCheck v
      in Result (UVar $ typedResult vRes) (errors vRes)

instance Checked Var where
  typeCheck v@(Var (TypedName typeLhs name) rhs) =
    let typeRhs = typeOf rhs
    in case typeLhs of
      TInferred _ -> Result (Var (TypedName typeRhs name) rhs) []
      _ -> Result v $ if typeLhs `assignableFrom` typeRhs then [] else [TypeConflict typeLhs typeRhs]

assignableFrom :: Type -> Type -> Bool
assignableFrom a b = a == b

typeOf :: Expr -> Type
typeOf e = case e of
  ELitBln _ -> TBln Mutable
  ELitChr _ -> TChr Mutable
  ELitFlt _ -> TFlt Mutable
  ELitInt _ -> TInt Mutable
  ELitStr _ -> TStr Mutable




