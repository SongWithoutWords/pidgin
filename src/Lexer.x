{
{-# OPTIONS_GHC -w #-}

module Lexer (scanTokens) where
import Tokens
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

@litInt                  = (0 | \-?[1-9][0-9]*)
@litFlt                  = @litInt \. [0-9]+

@nameLower               = $alphaLower $alphaNumeral*
@nameUpper               = $alphaUpper $alphaNumeral*

tokens                   :-

"--".*                   ; -- Comments

^ $spaceOrTab* /$display { checkIndentAction }

$white                   ;

Bln                      { appendTokenAction TknTypeBln }
Chr                      { appendTokenAction TknTypeChr }
Flt                      { appendTokenAction TknTypeFlt }
Int                      { appendTokenAction TknTypeInt }
Nat                      { appendTokenAction TknTypeNat }
Str                      { appendTokenAction TknTypeStr }
This                     { appendTokenAction TknTypeThis }

if                       { appendTokenAction TknIf }
else                     { appendTokenAction TknElse }
true                     { appendTokenAction TknTrue }
false                    { appendTokenAction TknFalse }
and                      { appendTokenAction TknAnd }
or                       { appendTokenAction TknOr }
not                      { appendTokenAction TknNot }
none                     { appendTokenAction TknNone }

namespace                { appendTokenAction TknNamespace }
class                    { appendTokenAction TknClass }

pub                      { appendTokenAction TknPub }
pro                      { appendTokenAction TknPro }
pri                      { appendTokenAction TknPri }

"~"                      { appendTokenAction TknTilde }
"@"                      { appendTokenAction TknAt }
"#"                      { appendTokenAction TknHash }
"$"                      { appendTokenAction TknDollar }
"^"                      { appendTokenAction TknCaret }
"&"                      { appendTokenAction TknAmpersand }
"*"                      { appendTokenAction TknStar }
"("                      { appendTokenAction TknLParen }
")"                      { appendTokenAction TknRParen }
"-"                      { appendTokenAction TknMinus }
"+"                      { appendTokenAction TknPlus }
"="                      { appendTokenAction TknEqual }
"["                      { appendTokenAction TknLBracket }
"]"                      { appendTokenAction TknRBracket }
";"                      { appendTokenAction TknSemicolon }
":"                      { appendTokenAction TknColon }
","                      { appendTokenAction TknComma }
"."                      { appendTokenAction TknDot }
"?"                      { appendTokenAction TknQMark }

"->"                     { appendTokenAction TknThinArrow }
"=>"                     { appendTokenAction TknFatArrow }

@litChar                 { lexTokenAction $ TknLitChr . read }
@litInt                  { lexTokenAction $ TknLitInt . read }
@litFlt                  { lexTokenAction $ TknLitFlt . read }
@litString               { lexTokenAction $ TknLitStr . init . tail }

@nameLower               { lexTokenAction $ TknName }
@nameUpper               { lexTokenAction $ TknTypename }


{
data AlexUserState = AlexUserState
  { tokens :: [Token]
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

appendTokens :: [Token] -> Update
appendTokens tkns state = state { tokens = (tokens state) ++ tkns}

appendTokensAction :: [Token] -> AlexAction ()
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
parseIndentation (UserInput str pos)
  | str == [] = 0
  | not $ alleq str = error $ lexError pos "mixed spaces and tabs indent"
  | head str == ' ' = if (length str `mod` spacesPerTab == 0)
    then length str `div` spacesPerTab
    else error $ lexError pos "indentation must occur in multiples of "++show spacesPerTab++" spaces"
  | head str == '\t' = length str
  | otherwise = error $ lexError pos "invalid indentation string \""++str++"\""

checkIndent' :: Int -> Update
checkIndent' indent state
  | indent < curIndent = setIndentDepth indent $ appendTokens ((replicate (curIndent - indent) TknDedent) ++ [TknEol]) state
  | indent == curIndent = appendToken TknEol state
  | indent == (curIndent + 1) = setIndentDepth indent $ appendToken TknIndent state
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

lexError :: AlexPosn -> String -> String
lexError pos msg = "Lexical error, line " ++ showLineNumber pos ++ ": " ++ msg


-- Interface
runAlexScan :: String -> Either ParseError AlexUserState
runAlexScan s = runAlex s $ alexMonadScan >> getUserState

scanTokens :: String -> [Token]
scanTokens s = case runAlexScan s of
  (Right state) -> tokens state ++ replicate (indentDepth state) TknDedent
  (Left parseError) -> error $ parseError
}
