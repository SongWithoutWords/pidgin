module TypeErrors where

import Types

import qualified Data.Set as Set

data TypeOrErrors
  = Type Type
  | Errors Errors
  deriving(Eq, Show)

getType :: TypeOrErrors -> Maybe Type
getType (Type t) = Just t
getType _ = Nothing

type Errors = [Error]

data Error

  = UnknownId String

  -- Better conflict or mismatch?
  | TypeConflict { typeRequired :: Type, typeFound :: Type }

  -- Could be called incompatible types? No common supertype?
  | FailedToUnify (Set.Set Type)

  | NonApplicable Type
  | WrongPurity { purityRequired :: Purity, purityFound :: Purity }
  | WrongNumArgs { numArgsRequired :: Int, numArgsFound :: Int }

  -- Should be multiple, competing, duplicate, overlapping, contrandictory?...
  | CompetingDefinitions

  | RecursiveDefinition

  | NeedExprFoundType
  | NeedExprFoundNamespace

  | ErrorPropagated Errors

  deriving(Eq, Show)

