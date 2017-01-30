{
{-# OPTIONS_GHC -w #-}

module Lexer (
Token(..),
scanTokens
) where

import Tokens
}

%wrapper "posn"

$digitNonZero = 1-9
$digit        = 0-9
$alphaLower   = [a-z]
$alphaUpper   = [A-Z]

$alpha        = [$alphaLower $alphaUpper]
$alphaNumeral = [$alpha $digit]
$eol          = [\n\;]

$whitespace   = [' ']

tokens :-


-- Comments
"--".*                                    ;

-- Syntax
$eol                                      { \_ _ -> TokenEol }

-- Should be able to limit to spaces and tabs!
$white+                                   ;

Bln                                       { \p _ -> TokenBln }
Chr                                       { \p _ -> TokenChr }
Flt                                       { \p _ -> TokenFlt }
Int                                       { \p _ -> TokenInt }
Nat                                       { \p _ -> TokenNat }
Str                                       { \p _ -> TokenStr }

none                                      { \p _ -> TokenNone }
true                                      { \p _ -> TokenTrue }
false                                     { \p _ -> TokenFalse }
and                                       { \p _ -> TokenAnd }
or                                        { \p _ -> TokenOr }
not                                       { \p _ -> TokenNot }

"->"                                      { \p _ -> TokenThinArrow }
"=>"                                      { \p _ -> TokenFatArrow }

-- Stand-in: Will improve lexer to use indentation
"{"                                       { \p _ -> TokenIndent }
"}"                                       { \p _ -> TokenDedent }

"("                                       { \p _ -> TokenLParen }
")"                                       { \p _ -> TokenRParen }
"["                                       { \p _ -> TokenLBracket }
"]"                                       { \p _ -> TokenRBracket }

"@"                                       { \p _ -> TokenAt }
"#"                                       { \p _ -> TokenHash }
"^"                                       { \p _ -> TokenCaret }
"&"                                       { \p _ -> TokenAmpersand }

"~"                                       { \p _ -> TokenTilde }
"?"                                       { \p _ -> TokenQMark }
"*"                                       { \p _ -> TokenStar }
"+"                                       { \p _ -> TokenPlus }
"-"                                       { \p _ -> TokenMinus }
"."                                       { \p _ -> TokenDot }

[\-]? $digitNonZero $digit*               { \p s -> TokenIntLit (read s) }
[\-]? $digitNonZero $digit* [\.] $digit+  { \p s -> TokenFltLit (read s) }
[\"]([\\][\"]|[^\"])*[\"]                 { \p s -> TokenStrLit (init (tail s)) }
[\'][\\]?[^\n\t][\']                      { \p s -> TokenChrLit (read s) }

$alphaUpper $alphaNumeral*                { \p s -> TokenType s }
$alphaLower $alphaNumeral*                { \p s -> TokenName s }
-- $alphaLower*                { \p s -> TokenName s }

{
scanTokens :: String -> [Token]
scanTokens = alexScanTokens
}


