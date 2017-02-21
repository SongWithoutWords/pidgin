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

$quote        = \"

@char         = \\? [^\n\t]
@litChar      = \' @char \'
@string       = ( \\ $quote | [^ $quote \n]) *
@litString    = $quote @string $quote

@litInt       = (0 | \-?[1-9][0-9]*)
@litFlt       = @litInt \. [0-9]+

@nameLower    = $alphaLower $alphaNumeral*
@nameUpper    = $alphaUpper $alphaNumeral*

tokens        :-

-- Comments
"--".*        ;

-- Syntax
\n+           { yield TokenEol }

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
"{"           { yield TokenIndent }
"}"           { yield TokenDedent }

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

modifyUserState :: (AlexUserState -> AlexUserState) -> Alex ()
modifyUserState f = Alex $ \s -> let current = alex_ust s
                                     new = f current
                                 in
                                     Right (s { alex_ust = new}, ())

getUserState :: Alex AlexUserState
getUserState = Alex $ \s -> Right (s, alex_ust s)

yieldFromStr :: (String -> Token) -> AlexAction ()
yieldFromStr lex =
  \(posn, prevChar, pending, s) len -> modifyUserState (push $ take len s) >> alexMonadScan
    where
      push :: String -> AlexUserState -> AlexUserState
      push str state = state { tokens = (tokens state) ++ [lex str] }

yield :: Token -> AlexAction ()
yield token = yieldFromStr (\_ -> token)


runAlexScan :: String -> Either ParseError AlexUserState
runAlexScan s = runAlex s $ alexMonadScan >> getUserState

scanTokens :: String -> [Token]
scanTokens s = case runAlexScan s of
           (Right state) -> tokens state
           (Left parseError) -> error $ parseError

}
