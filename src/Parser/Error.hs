module Parser.Error where

import Lexer.Token
import FormatAst

parseError :: [Token] -> a
parseError tokens = error $ "Parse error: unexpected tokens " ++ formatTokens tokens

