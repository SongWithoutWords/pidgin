-- Asts are numbered sequentially, and represent the program at different phases of compilation
-- To understand the changes between different phases of compilation, it's best to diff the Asts
-- Ast3 represents the Ast after it has been type checked
module Ast.A3Typed
  ( module Ast.A2Constrained
  ) where

-- At present they're the same
import Ast.A2Constrained

