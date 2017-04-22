module TypeErrors where

data TypeError
  = UnknownId String
  | IncompatibleType
  deriving(Eq, Show)

type TypeErrors = [TypeError]
