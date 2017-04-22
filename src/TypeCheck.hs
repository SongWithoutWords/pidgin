module TypeCheck where

-- The type resolver. I've considered TypeCheck as a name, but that doesn't really express the importance of this system
-- in resolving type inference as well.

import Ast
import TypeErrors

data Result = Result { ast::Ast, errors::TypeErrors }

typeCheck :: Ast -> Result
typeCheck _ = Result [] []



