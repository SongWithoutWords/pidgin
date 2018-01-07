module Ast.Common.Intrinsic where

import Ast.A2Constrained.Type
import Ast.Common.Name

data Intrinsic
  = INeg
  | IAdd
  | ISub
  | IMul
  | IDiv
  | IRem
  | IMod

  | IEql
  | INeq
  | IGrt
  | ILsr
  | IGeq
  | ILeq

  | FNeg
  | FAdd
  | FSub
  | FMul
  | FDiv

  | FEql
  | FNeq
  | FGrt
  | FLsr
  | FGeq
  | FLeq

  | BNot
  | BAnd
  | BOrr

  | BEql
  | BNeq

  | ArrayCons
  deriving(Enum, Eq, Ord, Show)

intrinsics :: [Intrinsic]
intrinsics = [IAdd ..]

nameOfIntrinsic :: Intrinsic -> Name
nameOfIntrinsic f = case f of

  INeg -> "-"
  IAdd -> "+"
  ISub -> "-"
  IMul -> "*"
  IDiv -> "/"
  IRem -> "rem"
  IMod -> "%"

  IEql -> "=="
  INeq -> "!="
  IGrt -> ">"
  ILsr -> "<"
  IGeq -> ">="
  ILeq -> "<="

  FNeg -> "-"
  FAdd -> "+"
  FSub -> "-"
  FMul -> "*"
  FDiv -> "/"

  FEql -> "=="
  FNeq -> "!="
  FGrt -> ">"
  FLsr -> "<"
  FGeq -> ">="
  FLeq -> "<="

  BNot -> "not"
  BAnd -> "and"
  BOrr -> "or"
  BEql -> "=="
  BNeq -> "!="

  ArrayCons -> "Array"

typeOfIntrinsic :: Intrinsic -> Type
typeOfIntrinsic f = case f of

  INeg -> [TInt] ~> TInt
  IAdd -> [TInt, TInt] ~> TInt
  ISub -> [TInt, TInt] ~> TInt
  IMul -> [TInt, TInt] ~> TInt
  IDiv -> [TInt, TInt] ~> TInt
  IRem -> [TInt, TInt] ~> TInt
  IMod -> [TInt, TInt] ~> TInt

  IEql -> [TInt, TInt] ~> TBln
  INeq -> [TInt, TInt] ~> TBln
  IGrt -> [TInt, TInt] ~> TBln
  ILsr -> [TInt, TInt] ~> TBln
  IGeq -> [TInt, TInt] ~> TBln
  ILeq -> [TInt, TInt] ~> TBln

  FNeg -> [TFlt] ~> TFlt
  FAdd -> [TFlt, TFlt] ~> TFlt
  FSub -> [TFlt, TFlt] ~> TFlt
  FMul -> [TFlt, TFlt] ~> TFlt
  FDiv -> [TFlt, TFlt] ~> TFlt

  FEql -> [TFlt, TFlt] ~> TBln
  FNeq -> [TFlt, TFlt] ~> TBln
  FGrt -> [TFlt, TFlt] ~> TBln
  FLsr -> [TFlt, TFlt] ~> TBln
  FGeq -> [TFlt, TFlt] ~> TBln
  FLeq -> [TFlt, TFlt] ~> TBln

  BNot -> [TBln] ~> TBln
  BAnd -> [TBln, TBln] ~> TBln
  BOrr -> [TBln, TBln] ~> TBln
  BEql -> [TBln, TBln] ~> TBln
  BNeq -> [TBln, TBln] ~> TBln

  ArrayCons -> [TInt, TInt] ~> TArray TInt

  where
    (~>) :: [Type] -> Type -> Type
    (~>) args ret = TFunc Pure args ret

