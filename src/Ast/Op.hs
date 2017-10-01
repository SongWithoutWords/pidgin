module Ast.Op where

data BinOp
  = And
  | Or
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Cmp Comparison
  | OpUser String
  deriving(Eq, Ord, Show)

data Comparison
  = Greater
  | Lesser
  | GreaterEq
  | LesserEq
  | Equal
  | NotEqual
  deriving(Eq, Ord, Show)

data UnOp
  = Not
  | Neg
  deriving(Eq, Show)

