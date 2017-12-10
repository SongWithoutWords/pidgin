module TypeCheck(typeCheckAst) where

import qualified Ast.A1PostParse as A1
import qualified Ast.A3Typed as A3

import TypeCheck.ConstraintGen
import TypeCheck.SubAst
import TypeCheck.Unify

typeCheckAst :: A1.Ast -> (A3.Ast, Errors)
typeCheckAst ast =
  let
    (ast', constraints, constrainErrs) = constrainAst ast
    (substitutions, unifyErrs) = unify constraints
    (ast'', substituionErrs) = subAst substitutions ast'
  in
    (ast'', constrainErrs ++ unifyErrs ++ substituionErrs)

