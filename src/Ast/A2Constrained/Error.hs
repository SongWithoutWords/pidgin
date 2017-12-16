module Ast.A2Constrained.Error where

import Ast.A2Constrained
import TypeCheck.Constraint

type Errors = [Error]

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

  | UndefinedOperator BinOp Type Type

  -- Multiple, competing, duplicate, overlapping, contrandictory?...
  | CompetingDefinitions

  | NeedExprFoundType
  | NeedExprFoundNamespace

  | RecursiveVariableDefinition (Named Var)
  | AssignmentToImmutableValue

  deriving(Eq, Show)

