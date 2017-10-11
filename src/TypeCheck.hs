module TypeCheck(typeCheckAst) where

import Ast

import TypeCheck.ConstraintGen
import TypeCheck.SubAst
import TypeCheck.Unify

typeCheckAst :: Ast1 -> (Ast2, Errors)
typeCheckAst ast =
  let (ast', constraints) = constrainAst ast in
  let (substitutions, errors) = runWriter $ unifyConstraints constraints in
  (subAst substitutions ast', errors)

