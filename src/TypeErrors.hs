module TypeErrors where

import Types

import qualified Data.Set as Set

data Error

  = UnknownId String

  -- Better conflict or mismatch?
  | TypeConflict { expected :: Type, received :: Type }

  -- Could be called incompatible types? No common supertype?
  | FailedToUnify (Set.Set Type)

  | NonApplicable Type
  | ArgCount { acExpected :: Int, acReceived :: Int }

  -- Should be multiple, competing, duplicate, overlapping, contrandictory?...
  | CompetingDefinitions

  | RecursiveDefinition

  | NeedExprFoundType
  | NeedExprFoundNamespace

  deriving(Eq, Show)

type Errors = [Error]

