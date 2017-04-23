{-# language FlexibleInstances #-}
{-# language TypeSynonymInstances #-}

module TypeCheck(typeCheck, Result(..)) where

import Preface

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
  typeCheck v@(Var ((mutLhs, typeLhs), name) rhs) =
    let typeRhs = findTypeOf rhs
    in case typeLhs of
      TInferred -> Result (Var (mutLhs & typeRhs, name) rhs) []
      _ -> Result v $ if typeLhs `assignableFrom` typeRhs then [] else [TypeConflict typeLhs typeRhs]

assignableFrom :: Type -> Type -> Bool
assignableFrom a b = a == b

findTypeOf :: Expr -> Type
findTypeOf e = case e of
  ELitBln _ -> TBln
  ELitChr _ -> TChr
  ELitFlt _ -> TFlt
  ELitInt _ -> TInt
  ELitStr _ -> TStr




