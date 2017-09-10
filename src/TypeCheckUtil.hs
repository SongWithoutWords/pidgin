{-# language GADTs #-}
module TypeCheckUtil where

import Ast
import TypeCheckM

typeOfExpr :: ExprT -> TypeT
typeOfExpr (ExprT t _) = t

-- Is type b assignable to type a?
(<~) :: TypeT -> TypeT -> TypeCheckM s ()
t1 <~ t2 = when (t1 /= t2) $ raise TypeConflict { typeRequired = t1, typeFound = t2 }

enforceOrInfer :: (Maybe TypeT) -> TypeT -> TypeCheckM s TypeT
enforceOrInfer (Just prescribed) assigned = do
  prescribed <~ assigned
  return prescribed
enforceOrInfer Nothing assigned = return assigned

unifyTypes :: [TypeT] -> TypeT
unifyTypes [] = TNone
unifyTypes (x:_) = x -- TODO: Do this right

