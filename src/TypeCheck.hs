module TypeCheck(typeCheckAst) where

import Ast

import TypeCheck.ConstraintGen
import TypeCheck.SubAst
import TypeCheck.Unify

typeCheckAst :: Ast1 -> (Ast2, Errors)
typeCheckAst ast =
  let (ast', constraints, constrainErrs) = constrainAst ast in
  let (substitutions, unifyErrs) = unify constraints in
  (subAst substitutions ast', constrainErrs ++ unifyErrs)

