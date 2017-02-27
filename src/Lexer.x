{
{-# OPTIONS_GHC -w #-}

module Lexer (scanTokens) where
import Tokens
}

%wrapper "monadUserState"


$digitNonZero = 1-9
$digit        = 0-9
$alphaLower   = a-z
$alphaUpper   = A-Z

$alpha        = [$alphaLower $alphaUpper]
$alphaNumeral = [$alpha $digit]

$space        = [\ ]
$tab          = \t
$spaceOrTab   = [$space $tab]
$display      = [^$white]

$quote        = \"

@char         = \\? [^\n\t]
@litChar      = \' @char \'
@string       = ( \\ $quote | [^ $quote \n]) *
@litString    = $quote @string $quote

@litInt       = (0 | \-?[1-9][0-9]*)
@litFlt       = @litInt \. [0-9]+

@nameLower    = $alphaLower $alphaNumeral*
@nameUpper    = $alphaUpper $alphaNumeral*

-- @indentSpaces = ^ $space+ /[^$white]
-- @indentTabs   = ^ $tab+   /[^$white]

tokens        :-

-- Comments
"--".*        ;

-- Syntax
\n+           { yield TokenEol }


^ $spaceOrTab* /$display { checkIndent }
-- @indentTabs   { yield TokenIndent }

-- Should be able to limit to spaces and tabs!
$white        ;


Bln           { yield TokenTypeBln }
Chr           { yield TokenTypeChr }
Flt           { yield TokenTypeFlt }
Int           { yield TokenTypeInt }
Nat           { yield TokenTypeNat }
Str           { yield TokenTypeStr }

if            { yield TokenIf } -- \p _ -> TokenIf }
else          { yield TokenElse }
true          { yield TokenTrue }
false         { yield TokenFalse }
and           { yield TokenAnd }
or            { yield TokenOr }
not           { yield TokenNot }
none          { yield TokenNone }

-- Stand-in: Will improve lexer to use indentation
-- "{"           { yield TokenIndent }
-- "}"           { yield TokenDedent }

"~"           { yield TokenTilde }
"@"           { yield TokenAt }
"#"           { yield TokenHash }
"$"           { yield TokenDollar }
"^"           { yield TokenCaret }
"&"           { yield TokenAmpersand }
"*"           { yield TokenStar }
"("           { yield TokenLParen }
")"           { yield TokenRParen }
"-"           { yield TokenMinus }
"+"           { yield TokenPlus }
"="           { yield TokenEqual }
"["           { yield TokenLBracket }
"]"           { yield TokenRBracket }
";"           { yield TokenSemicolon }
":"           { yield TokenColon }
","           { yield TokenComma }
"."           { yield TokenDot }
"?"           { yield TokenQMark }

"->"          { yield TokenThinArrow }
"=>"          { yield TokenFatArrow }

@litChar      { yieldFromStr $ TokenLitChr . read }
@litInt       { yieldFromStr $ TokenLitInt . read }
@litFlt       { yieldFromStr $ TokenLitFlt . read }
@litString    { yieldFromStr $ TokenLitStr . init . tail }

@nameLower    { yieldFromStr $ TokenName }
@nameUpper    { yieldFromStr $ TokenTypeName }

{
data AlexUserState = AlexUserState
  { tokens :: [Token]
  , indChar :: Char
  , indMult :: Int
  , indDepth :: Int }

type ParseError = String

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState [] ' '0 0

alexEOF :: Alex()
alexEOF = return ()

ignore input len = alexMonadScan

getUserState :: Alex AlexUserState
getUserState = Alex $ \s -> Right (s, alex_ust s)

type Update = (AlexUserState -> AlexUserState)

updateUserState :: Update -> Alex ()
updateUserState update = (Alex $ \state ->
  let current = alex_ust state
      next = update current
      in Right (state { alex_ust = next}, ())) >> alexMonadScan

actionUpdate :: Update -> AlexAction ()
actionUpdate update = \_ _ -> updateUserState update

data UserInput = UserInput
  { string :: String
  , posn :: AlexPosn }

type UpdateWithInput = UserInput -> Update

alexToUserInput :: AlexInput -> Int -> UserInput
alexToUserInput (posn, prevChar, pending, s) len = UserInput (take len s) posn

actionInputUpdate :: UpdateWithInput -> AlexAction ()
actionInputUpdate updateWithInput = \alexInput len -> updateUserState $ updateWithInput (alexToUserInput alexInput len)


-- transformUserState :: (AlexUserState -> AlexUserState) -> Alex ()
-- transformUserState transform = (modifyUserState transform) >> alexMonadScan

appendTokens :: [Token] -> (AlexUserState -> AlexUserState)
appendTokens newTokens state = state { tokens = (tokens state) ++ newTokens}

yieldFromStr :: (String -> Token) -> AlexAction ()
yieldFromStr lex =
  \(posn, prevChar, pending, s) len -> updateUserState (appendTokens [lex $ take len s])

yield :: Token -> AlexAction ()
yield token = actionUpdate $ appendTokens [token]

lineNumber :: AlexPosn -> Int
lineNumber (AlexPn charOffset line column) = line

checkIndent :: AlexAction ()
checkIndent (posn, prevChar, pending, s) len = updateUserState (checkIndent' $ take len s)
  where
    checkIndent' :: String -> AlexUserState -> AlexUserState
    checkIndent' str state
      | str == [] = appendTokens (replicate (indDepth state) TokenDedent) state
      | not $ alleq str = error $ "Mixed spaces and tabs in indentation of line " ++ show (lineNumber posn)
      | otherwise = error ""

alleq :: Eq a => [a] -> Bool
alleq [] = True
alleq (x:xs)
  | xs == [] = True
  | x == head xs = alleq xs
  | otherwise = False

runAlexScan :: String -> Either ParseError AlexUserState
runAlexScan s = runAlex s $ alexMonadScan >> getUserState

scanTokens :: String -> [Token]
scanTokens s = case runAlexScan s of
           (Right state) -> tokens state
           (Left parseError) -> error $ parseError

}
