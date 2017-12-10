module Ast.Common.Purity where

data Purity
  = Pure
  | PRead
  | PWrite
  deriving(Eq, Ord, Show)

