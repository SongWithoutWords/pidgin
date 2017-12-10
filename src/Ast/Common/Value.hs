module Ast.Common.Value where

data Value
  = VBln Bool
  | VChr Char
  | VFlt Float
  | VInt Int
  | VStr String
  deriving(Eq, Show)

