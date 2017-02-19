{
{-# OPTIONS_GHC -w #-}

module Lexer ({-Token(..),-} scanTokens) where
import Tokens
}

%wrapper "posn"

$digitNonZero = 1-9
$digit        = 0-9
$alphaLower   = a-z
$alphaUpper   = A-Z

$alpha        = [$alphaLower $alphaUpper]
$alphaNumeral = [$alpha $digit]

@char         = \\? [^\n\t]
@litChar      = \' @char \'
@string       = (\\\"|[^\"\n])*
@litString    = \" @string \"

@litInt       = (0 | \-?[1-9][0-9]*)
@litFlt       = @litInt \. [0-9]+

@nameLower    = $alphaLower $alphaNumeral*
@nameUpper    = $alphaUpper $alphaNumeral*

tokens        :-

-- Comments
"--".*        ;

-- Syntax
\n+           { \_ _ -> TokenEol }

-- Should be able to limit to spaces and tabs!
$white        ;

Bln           { \p _ -> TokenTypeBln }
Chr           { \p _ -> TokenTypeChr }
Flt           { \p _ -> TokenTypeFlt }
Int           { \p _ -> TokenTypeInt }
Nat           { \p _ -> TokenTypeNat }
Str           { \p _ -> TokenTypeStr }

if            { \p _ -> TokenIf }
else          { \p _ -> TokenElse }
true          { \p _ -> TokenTrue }
false         { \p _ -> TokenFalse }
and           { \p _ -> TokenAnd }
or            { \p _ -> TokenOr }
not           { \p _ -> TokenNot }
none          { \p _ -> TokenNone }

-- Stand-in: Will improve lexer to use indentation
"{"           { \p _ -> TokenIndent }
"}"           { \p _ -> TokenDedent }

"~"           { \p _ -> TokenTilde }
"@"           { \p _ -> TokenAt }
"#"           { \p _ -> TokenHash }
"$"           { \p _ -> TokenDollar }
"^"           { \p _ -> TokenCaret }
"&"           { \p _ -> TokenAmpersand }
"*"           { \p _ -> TokenStar }
"("           { \p _ -> TokenLParen }
")"           { \p _ -> TokenRParen }
"-"           { \p _ -> TokenMinus }
"+"           { \p _ -> TokenPlus }
"="           { \p _ -> TokenEqual }
"["           { \p _ -> TokenLBracket }
"]"           { \p _ -> TokenRBracket }
";"           { \p _ -> TokenSemicolon }
":"           { \p _ -> TokenColon }
","           { \p _ -> TokenComma }
"."           { \p _ -> TokenDot }
"?"           { \p _ -> TokenQMark }

"->"          { \p _ -> TokenThinArrow }
"=>"          { \p _ -> TokenFatArrow }

@litChar      { \p s -> TokenLitChr (read s) }
@litInt       { \p s -> TokenLitInt (read s) }
@litFlt       { \p s -> TokenLitFlt (read s) }
@litString    { \p s -> TokenLitStr (init (tail s)) }

@nameLower    { \p s -> TokenName s }
@nameUpper    { \p s -> TokenTypeName s }

{
scanTokens :: String -> [Token]
scanTokens = alexScanTokens
}

