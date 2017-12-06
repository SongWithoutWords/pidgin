{
{-# OPTIONS_GHC -w #-}

module Lexer (scanTokens) where
import qualified Tokens as T
}

%wrapper "monadUserState"

$digitNonZero            = 1-9
$digit                   = 0-9
$alphaLower              = a-z
$alphaUpper              = A-Z

$alpha                   = [$alphaLower $alphaUpper]
$alphaNumeral            = [$alpha $digit]

$space                   = [\ ]
$tab                     = \t
$spaceOrTab              = [$space $tab]
$display                 = [^$white]

$quote                   = \"

@char                    = \\? [^\n\t]
@litChar                 = \' @char \'
@string                  = ( \\ $quote | [^ $quote \n]) *
@litString               = $quote @string $quote

@litInt                  = (0 | [1-9][0-9]*)
@litFlt                  = @litInt \. [0-9]+

@nameLower               = $alphaLower $alphaNumeral*
@nameUpper               = $alphaUpper $alphaNumeral*

tokens                   :-

"--".*                   ; -- Comments

\n+ $spaceOrTab* /$display { checkIndentAction }

$white                   ;

Bln                      { appendTokenAction T.TypeBln }
Chr                      { appendTokenAction T.TypeChr }
Flt                      { appendTokenAction T.TypeFlt }
Int                      { appendTokenAction T.TypeInt }
Nat                      { appendTokenAction T.TypeNat }
None                     { appendTokenAction T.TypeNone }
Str                      { appendTokenAction T.TypeStr }
This                     { appendTokenAction T.TypeThis }

if                       { appendTokenAction T.If }
then                     { appendTokenAction T.Then }
else                     { appendTokenAction T.Else }
true                     { appendTokenAction T.True }
false                    { appendTokenAction T.False }
and                      { appendTokenAction T.And }
or                       { appendTokenAction T.Or }
not                      { appendTokenAction T.Not }
none                     { appendTokenAction T.None }
ret                      { appendTokenAction T.Ret }

namespace                { appendTokenAction T.Namespace }
class                    { appendTokenAction T.Class }

pub                      { appendTokenAction T.Pub }
pro                      { appendTokenAction T.Pro }
pri                      { appendTokenAction T.Pri }

"~"                      { appendTokenAction T.Tilde }
"@"                      { appendTokenAction T.At }
"#"                      { appendTokenAction T.Hash }
"$"                      { appendTokenAction T.Dollar }
"%"                      { appendTokenAction T.Percent }
"^"                      { appendTokenAction T.Caret }
"&"                      { appendTokenAction T.Ampersand }
"*"                      { appendTokenAction T.Star }
"("                      { appendTokenAction T.LParen }
")"                      { appendTokenAction T.RParen }
"-"                      { appendTokenAction T.Minus }
"+"                      { appendTokenAction T.Plus }
"="                      { appendTokenAction T.Equal }
"["                      { appendTokenAction T.LBracket }
"]"                      { appendTokenAction T.RBracket }
";"                      { appendTokenAction T.Semicolon }
":"                      { appendTokenAction T.Colon }
","                      { appendTokenAction T.Comma }
"."                      { appendTokenAction T.Dot }
"?"                      { appendTokenAction T.QMark }
"/"                      { appendTokenAction T.Slash}

"<"                      { appendTokenAction T.Lesser }
">"                      { appendTokenAction T.Greater }
"<="                     { appendTokenAction T.LesserEq }
">="                     { appendTokenAction T.GreaterEq }
"=="                     { appendTokenAction T.EqualEqual }
"!="                     { appendTokenAction T.NotEqual }

"->"                     { appendTokenAction T.ThinArrow }
"=>"                     { appendTokenAction T.FatArrow }

@litChar                 { lexTokenAction $ T.LitChr . read }
@litInt                  { lexTokenAction $ T.LitInt . read }
@litFlt                  { lexTokenAction $ T.LitFlt . read }
@litString               { lexTokenAction $ T.LitStr . init . tail }

@nameLower               { lexTokenAction $ T.Name }
@nameUpper               { lexTokenAction $ T.Typename }


{
type Token = T.Token
type Tokens = T.Tokens

data AlexUserState = AlexUserState
  { tokens :: Tokens
  , indentDepth :: Int }

type ParseError = String

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState [] 0

alexEOF :: Alex ()
alexEOF = return ()

ignore input len = alexMonadScan

getUserState :: Alex AlexUserState
getUserState = Alex $ \s -> Right (s, alex_ust s)

updateUserState :: Update -> Alex ()
updateUserState update = (Alex $ \state ->
  let current = alex_ust state
      next = update current
      in Right (state { alex_ust = next}, ())) >> alexMonadScan


-- Update and types of updates
type Update = (AlexUserState -> AlexUserState)

updateAction :: Update -> AlexAction ()
updateAction update _ _ = updateUserState update

appendToken :: Token -> Update
appendToken tkn state = state { tokens = (tokens state) ++ [tkn]}

appendTokenAction :: Token -> AlexAction ()
appendTokenAction = updateAction . appendToken

appendTokens :: Tokens -> Update
appendTokens tkns state = state { tokens = (tokens state) ++ tkns}

appendTokensAction :: Tokens -> AlexAction ()
appendTokensAction = updateAction . appendTokens

setIndentDepth :: Int -> Update
setIndentDepth n state = state { indentDepth = n }


-- Updates with intput
data UserInput = UserInput
  { str :: String
  , pos :: AlexPosn }

alexToUserInput :: AlexInput -> Int -> UserInput
alexToUserInput (posn, prevChar, pending, s) len = UserInput (take len s) posn

type UserInputUpdate = UserInput -> Update

updateInputAction :: UserInputUpdate -> AlexAction ()
updateInputAction userInputUpdate alexInput len = updateUserState $ userInputUpdate $ alexToUserInput alexInput len

lexToken :: (String -> Token) -> UserInputUpdate
lexToken lex (UserInput str _) = appendToken $ lex str

lexTokenAction :: (String -> Token) -> AlexAction ()
lexTokenAction = updateInputAction . lexToken

spacesPerTab = 4 -- TODO: Make adjustable by user

parseIndentation :: UserInput -> Int
parseIndentation (UserInput strWithNewlines pos) = parseIndentation' (dropWhile (=='\n') strWithNewlines) pos

parseIndentation' :: String -> AlexPosn -> Int
parseIndentation' str pos
  | str == [] = 0
  | not $ alleq str = error $ lexError pos "mixed spaces and tabs indent"
  | head str == ' ' = if (strLength `mod` spacesPerTab == 0)
    then strLength `div` spacesPerTab
    else error $ lexError pos
      "indentation must occur in multiples of "++show spacesPerTab++" spaces, found " ++ show strLength
  | head str == '\t' = strLength
  | otherwise = error $ lexError pos "invalid indentation string \""++str++"\""
    where strLength = length str

checkIndent' :: Int -> Update
checkIndent' indent state
  | indent < curIndent = setIndentDepth indent $ appendTokens ((replicate (curIndent - indent) T.Dedent) ++ [T.Eol]) state
  | indent == curIndent = appendToken T.Eol state
  | indent == (curIndent + 1) = setIndentDepth indent $ appendToken T.Indent state
  | indent == (curIndent + 2) = state -- line continuation, parser need not be aware
    where
      curIndent = indentDepth state

checkIndent :: UserInputUpdate
checkIndent = checkIndent' . parseIndentation

checkIndentAction :: AlexAction ()
checkIndentAction = updateInputAction checkIndent


-- Utility functions
alleq :: Eq a => [a] -> Bool
alleq [] = True
alleq (x:xs)
  | xs == [] = True
  | x == head xs = alleq xs
  | otherwise = False

lineNumber :: AlexPosn -> Int
lineNumber (AlexPn charOffset line column) = line

showLineNumber pos = show $ lineNumber pos

-- TODO: Display column in addition to row in error message

lexError :: AlexPosn -> String -> String
lexError pos msg = "Lexical error, line " ++ showLineNumber pos ++ ": " ++ msg


-- Interface
runAlexScan :: String -> Either ParseError AlexUserState
runAlexScan s = runAlex s $ alexMonadScan >> getUserState

scanTokens :: String -> [Token]
scanTokens s = case runAlexScan s of
  (Right state) -> tokens state ++ replicate (indentDepth state) T.Dedent
  (Left parseError) -> error $ parseError
}
