module TypeErrors where

-- TODO: May as well rexport types from Ast now
-- import Ast
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

  | RecursiveDefinition (Set.Set String) -- should really be names

  | NeedExprFoundType
  | NeedExprFoundNamespace

  | ErrorPropagated Errors

  deriving(Eq, Show)

recursiveDefinition :: [String] -> Error
recursiveDefinition = RecursiveDefinition . Set.fromList

