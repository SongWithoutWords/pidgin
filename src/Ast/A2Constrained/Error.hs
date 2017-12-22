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
  | WrongNumArgs { numArgsRequired :: Int, numArgsFound :: Int }

  -- Multiple, competing, duplicate, overlapping, contrandictory?...
  | CompetingDefinitions

  | NeedExprFoundType
  | NeedExprFoundNamespace

  | RecursiveVariableDefinition Name
  | AssignmentToImmutableValue

  deriving(Eq, Ord, Show)

