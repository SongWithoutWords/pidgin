module Transforms
  ( lexParseCheck
  ) where

import System.IO.Unsafe

import Ast
import MultiMapAst
import TypeErrors

import Lexer
import Parser
import TypeCheck

(|>) :: (a -> b) -> (b -> c) -> (a -> c)
f |> g = g . f

lexParseCheck :: String -> (AstMc, Errors)
lexParseCheck = scanTokens |> parse |> multiMapAst |> typeCheckAst
