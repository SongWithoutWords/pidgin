module TypeErrors where

import Ast

data TypeError
  = UnknownId String
  | TypeConflict Type Type
  deriving(Eq, Show)

type TypeErrors = [TypeError]
