module Ast.A2Constrained.Error where

import Ast.A2Constrained
import Util.Preface
import Util.UnorderedPair

type Errors = [Error]

data Error

  = ImplicitRetWithoutFinalExpr
  | MidBlockReturnStatement
  | UselessExpression

  | UnknownId Name

  | UnknownTypeName Typename
  | AmbiguousTypeName Typename

  | FailedToUnify (UnorderedPair Type)
  | FailedToInferType Type

  | NonApplicable Type
  | WrongPurity { purityRequired :: Purity, purityFound :: Purity }
  | WrongNumArgs { numArgsRequired :: Int, numArgsFound :: Int }

  | UndefinedOperator BinOp Type Type

  -- Multiple, competing, duplicate, overlapping, contrandictory?...
  | CompetingDefinitions

  | NeedExprFoundType
  | NeedExprFoundNamespace

  | RecursiveVariableDefinition (Named Var)

  -- | Propagated

  deriving(Eq, Show)

failedToUnify :: Type -> Type -> Error
failedToUnify = FailedToUnify .: UnorderedPair

