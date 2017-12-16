module TypeCheck.Substitution where

import qualified Data.Map as M

import Ast.A2Constrained.Type

type Substitutions = M.Map TVar Type


