{-# language GADTs #-}
module TypeCheckUtil where

import Ast
import TypeCheckM

typeOfExpr :: Expr2 -> Type2
typeOfExpr (Expr2 t _) = t

-- Is type b assignable to type a?
(<~) :: Type2 -> Type2 -> TypeCheckM s ()
t1 <~ t2 = when (t1 /= t2) $ raise TypeConflict { typeRequired = t1, typeFound = t2 }

enforceOrInfer :: (Maybe Type2) -> Type2 -> TypeCheckM s Type2
enforceOrInfer (Just prescribed) assigned = do
  prescribed <~ assigned
  return prescribed
enforceOrInfer Nothing assigned = return assigned

unifyTypes :: [Type2] -> Type2
unifyTypes [] = TNone
unifyTypes (x:_) = x -- TODO: Do this right

