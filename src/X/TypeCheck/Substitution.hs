module TypeCheck.Substitution where

-- import qualified Data.Map as M
import qualified Data.IntMap.Lazy as M

import Ast.A2Constrained

type Substitutions = M.IntMap Type


