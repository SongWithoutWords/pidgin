module Ast.A2Constrained.Error where

import Data.Set

import Ast.A2Constrained
import TypeCheck.Constraint

type Errors = Set Error

data Error

  = ImplicitRetWithoutFinalExpr
  | MidBlockReturnStatement
  | UselessExpression

  | UnknownId Name

  | UnknownTypeName Typename
  | AmbiguousTypeName Typename

  | FailedToUnify Constraint
  | FailedToInferType Type
  | FailedUnification Constraints

  | NonApplicable Type
  | WrongPurity { purityRequired :: Purity, purityFound :: Purity }
  | WrongMutability
  | WrongNumArgs { numArgsRequired :: Int, numArgsFound :: Int }
  | NoViableOverload

  -- Multiple, competing, duplicate, overlapping, contrandictory?...
  | EquallyViableOverloads Type Types

  | NeedExprFoundType
  | NeedExprFoundNamespace
  | NoExpressionWithName Name

  | RecursiveVariableDefinition Name
  | AssignmentToImmutableValue

  deriving(Eq, Ord, Show)

