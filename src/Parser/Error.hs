module Parser.Error where

import Tokens
import FormatAst

parseError :: [Token] -> a
parseError tokens = error $ "Parse error: unexpected tokens " ++ formatTokens tokens

