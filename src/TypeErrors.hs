module TypeErrors where

import Ast

data Error
  = UnknownId String
  | TypeConflict { expected :: Type, received :: Type }
  deriving(Eq, Show)

type Errors = [Error]

