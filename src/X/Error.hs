module X.Error where

import Data.Set

import X.Ast
import TypeCheck.Constraint

type Errors = Set Error

data Error

  = MidBlockReturn
  | UselessExpression

  | UnknownId Name

  | UnknownTypeName Typename
  | AmbiguousTypeName Typename
  | UnknownMemberVariable Typename Name
  | AmbigousMemberVariable Typename Name

  | WrongType { typeExpected :: Type, typeFound :: Type }
  | FailedToInferType Type
  | FailedUnification Constraints

  | NonApplicable Type
  | WrongPurity { purityExpected :: Purity, purityFound :: Purity }
  | WrongMutability
  | WrongNumArgs { numArgsExpected :: Int, numArgsFound :: Int }
  | NoViableOverload

  -- Multiple, competing, duplicate, overlapping, contrandictory?...
  | EquallyViableOverloads Type (Set Type)

  | NeedExprFoundType
  | NeedExprFoundNamespace
  | NoExpressionWithName Name

  | RecursiveVariableDefinition Name
  | AssignmentToImmutableValue

  deriving(Eq, Ord, Show)

