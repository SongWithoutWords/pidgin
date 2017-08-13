module Transforms
  ( lexParseCheck
  ) where

import System.IO.Unsafe

import Ast
import TypeErrors

import Lexer
import Parser
import TypeCheck

(|>) :: (a -> b) -> (b -> c) -> (a -> c)
f |> g = g . f

-- temporary: doesn't really belong here
lexParseCheck :: String -> (AstMc, Errors)
lexParseCheck = scanTokens |> parse |> typeCheckAst --(unsafePerformIO . typeCheckAst)
