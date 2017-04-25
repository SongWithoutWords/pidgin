module TypeErrors where

import Ast

data Error
  = UnknownId String
  | TypeConflict { typeExpected :: Type,  typeReceived:: Type }
  deriving(Eq, Show)

type Errors = [Error]

