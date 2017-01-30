module ParseError where 

import Tokens

parseError :: [Token] -> a
parseError tokens = error $ "Parse error: unexpected token " ++ show tokens

