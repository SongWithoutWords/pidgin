module TypeErrors where

import Types

import qualified Data.Set as Set

data Error
  = UnknownId String
  | TypeConflict { expected :: Type, received :: Type }
  | FailedToUnify (Set.Set Type)
  | NonApplicable Type
  | ArgCount { acExpected :: Int, acReceived :: Int }
  deriving(Eq, Show)

type Errors = [Error]

