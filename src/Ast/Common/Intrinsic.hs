module Ast.Common.Intrinsic where

import Ast.A2Constrained.Type
import Ast.Common.Name

data Intrinsic
  = IAdd
  | ISub
  | IMul
  | IDiv
  | INeg

  | IEql
  | INeq
  | IGrt
  | ILsr
  | IGeq
  | ILeq

  | FAdd
  | FSub
  | FMul
  | FDiv
  | FNeg

  | FEql
  | FNeq
  | FGrt
  | FLsr
  | FGeq
  | FLeq

  | BAnd
  | BOrr
  | BNot

  | BEql
  | BNeq
  deriving(Eq, Ord, Show)

nameOfIntrinsic :: Intrinsic -> Name
nameOfIntrinsic f = case f of

  IAdd -> "+"
  ISub -> "-"
  IMul -> "*"
  IDiv -> "/"
  INeg -> "-"

  IEql -> "=="
  INeq -> "!="
  IGrt -> ">"
  ILsr -> "<"
  IGeq -> ">="
  ILeq -> "<="

  FAdd -> "+"
  FSub -> "-"
  FMul -> "*"
  FDiv -> "/"
  FNeg -> "-"

  FEql -> "=="
  FNeq -> "-"
  FGrt -> ">"
  FLsr -> "<"
  FGeq -> ">="
  FLeq -> "<="

  BAnd -> "and"
  BOrr -> "or"
  BEql -> "=="
  BNeq -> "!="
  BNot -> "not"

(~>) :: [Type] -> Type -> Type
(~>) args ret = TFunc Pure args ret

typeOfIntrinsic :: Intrinsic -> Type
typeOfIntrinsic f = case f of

  IAdd -> [TInt, TInt] ~> TInt
  ISub -> [TInt, TInt] ~> TInt
  IMul -> [TInt, TInt] ~> TInt
  IDiv -> [TInt, TInt] ~> TInt
  INeg -> [TInt] ~> TInt

  IEql -> [TInt, TInt] ~> TBln
  INeq -> [TInt, TInt] ~> TBln
  IGrt -> [TInt, TInt] ~> TBln
  ILsr -> [TInt, TInt] ~> TBln
  IGeq -> [TInt, TInt] ~> TBln
  ILeq -> [TInt, TInt] ~> TBln

  FAdd -> [TFlt, TFlt] ~> TFlt
  FSub -> [TFlt, TFlt] ~> TFlt
  FMul -> [TFlt, TFlt] ~> TFlt
  FDiv -> [TFlt, TFlt] ~> TFlt
  FNeg -> [TFlt] ~> TFlt

  FEql -> [TFlt, TFlt] ~> TBln
  FNeq -> [TFlt, TFlt] ~> TBln
  FGrt -> [TFlt, TFlt] ~> TBln
  FLsr -> [TFlt, TFlt] ~> TBln
  FGeq -> [TFlt, TFlt] ~> TBln
  FLeq -> [TFlt, TFlt] ~> TBln

  BAnd -> [TBln, TBln] ~> TBln
  BOrr -> [TBln, TBln] ~> TBln
  BEql -> [TBln, TBln] ~> TBln
  BNeq -> [TBln, TBln] ~> TBln
  BNot -> [TBln] ~> TBln

