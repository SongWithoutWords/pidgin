module X.TypeCheck where

import Data.Monoid((<>))

import qualified Ast.A1PostParse as A1
-- import qualified Ast.A3Typed as A3

import X.Ast
import X.TypeCheck.ConstraintGen
import X.TypeCheck.Reduce
import X.Error
-- import TypeCheck.SubAst
-- import TypeCheck.Unify

-- typeCheckAst :: A1.Ast -> (Ast, Errors)
-- typeCheckAst ast = reduceAst $ constrainAst ast

  -- let
    -- (ast', constraints, constrainErrs) = constrainAst ast
    -- (substitutions, unifyErrs) = unify constraints
    -- (ast'', substituionErrs) = subAst substitutions ast'
  -- in
    -- (ast'', constrainErrs <> unifyErrs <> substituionErrs)

