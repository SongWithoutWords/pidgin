module TypeErrors where

import Ast

data Error
  = UnknownId String
  | TypeConflict { expected :: Type, received :: Type }
  | NonApplicable Type
  | ArgCount { acExpected :: Int, acReceived :: Int }
  deriving(Eq, Show)

type Errors = [Error]

