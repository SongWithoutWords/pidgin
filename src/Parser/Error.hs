module Parser.Error where

import Lexer.Token
import Lexer.FormatTokens

parseError :: [Token] -> a
parseError tokens = error $ "Parse error: unexpected tokens " ++ show tokens

