{
module Parser where
import Tokens
import Syntax
import ParseError
}

%name pidgin
%tokentype { Token }
%error { parseError }

%token
  eol      { TokenEol }
  Bln      { TokenBln }
  Chr      { TokenChr }
  Flt      { TokenFlt }
  Int      { TokenInt }
  Nat      { TokenNat }
  Str      { TokenStr }
  none     { TokenNone }
  true     { TokenTrue }
  false    { TokenFalse }
  and      { TokenAnd }
  or       { TokenOr }
  not      { TokenNot }
  "->"     { TokenThinArrow }
  "=>"     { TokenFatArrow }
  ind      { TokenIndent }
  ded      { TokenDedent }
  "("      { TokenLParen }
  ")"      { TokenRParen }
  "["      { TokenLBracket }
  "]"      { TokenRBracket }
  "@"      { TokenAt }
  "#"      { TokenHash }
  "^"      { TokenCaret }
  "&"      { TokenAmpersand }
  "~"      { TokenTilde }
  "?"      { TokenQMark }
  "*"      { TokenStar }
  "+"      { TokenPlus }
  "-"      { TokenMinus }
  "."      { TokenDot }
  litChr   { TokenChrLit $$ }
  litFlt   { TokenFltLit $$ }
  litInt   { TokenIntLit $$ }
  litStr   { TokenStrLit $$ }
  type     { TokenType $$ }
  name     { TokenName $$ }

%%

file : exprs eol { $1 }

exprs
  : {- empty -} { [] }
  | exprs expr { $2 : $1 }

expr
  : name { Variable $1 }
  | name "(" exprs ")" { FuncInvoke $1 $3 } 
  | expr "." name "(" exprs ")" { MethInvoke $1 $3 $5 }
  | lit { Lit $1 }

lit
  : litChr {LitChr $1}
  | litFlt {LitFlt $1}
  | litInt {LitInt $1}
  | litStr {LitStr $1}


