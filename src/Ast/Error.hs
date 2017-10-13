module Ast.Error where

import Ast.Name
import Ast.Op
import Ast.Type
import Cycle
import Preface
import Util.UnorderedPair

type Errors = [Error]

data Error

  = ImplicitRetWithoutFinalExpr
  | MidBlockReturnStatement
  | UselessExpression

  | UnknownId Name

  | UnknownTypeName Typename
  | AmbiguousTypeName Typename

  | FailedToUnify (UnorderedPair Type2)

  | NonApplicable Type2
  | WrongPurity { purityRequired :: Purity, purityFound :: Purity }
  | WrongNumArgs { numArgsRequired :: Int, numArgsFound :: Int }

  | UndefinedOperator BinOp Type2 Type2

  -- Multiple, competing, duplicate, overlapping, contrandictory?...
  | CompetingDefinitions

  | RecursiveDefinition (Cycle Name)

  | NeedExprFoundType
  | NeedExprFoundNamespace

  | Propagated

  deriving(Eq, Show)

failedToUnify :: Type2 -> Type2 -> Error
failedToUnify = FailedToUnify .: UnorderedPair

recursiveDefinition :: [Name] -> Error
recursiveDefinition = RecursiveDefinition . Cycle

