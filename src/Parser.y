{
module Parser where
import Tokens
import Syntax
import ParseError
}

%name parse
%tokentype { Token }
%error { parseError }

%token
  eol           { TokenEol }

  Bln           { TokenTypeBln }
  Chr           { TokenTypeChr }
  Flt           { TokenTypeFlt }
  Int           { TokenTypeInt }
  Nat           { TokenTypeNat }
  Str           { TokenTypeStr }

  if            { TokenIf }
  else          { TokenElse }
  true          { TokenTrue }
  false         { TokenFalse }
  and           { TokenAnd }
  or            { TokenOr }
  not           { TokenNot }
  none          { TokenNone }

  ind           { TokenIndent }
  ded           { TokenDedent }

  "~"           { TokenTilde }
  "@"           { TokenAt }
  "#"           { TokenHash }
  "$"           { TokenDollar }
  "^"           { TokenCaret }
  "&"           { TokenAmpersand }
  "*"           { TokenStar }
  "("           { TokenLParen }
  ")"           { TokenRParen }
  "-"           { TokenMinus }
  "+"           { TokenPlus }
  "="           { TokenEqual }
  "["           { TokenLBracket }
  "]"           { TokenRBracket }
  ";"           { TokenSemicolon}
  ":"           { TokenColon }
  ","           { TokenComma }
  "."           { TokenDot }
  "?"           { TokenQMark }

  "->"          { TokenThinArrow }
  "=>"          { TokenFatArrow }

  litChr        { TokenLitChr $$ }
  litFlt        { TokenLitFlt $$ }
  litInt        { TokenLitInt $$ }
  litStr        { TokenLitStr $$ }

  tokenName     { TokenName $$ }
  tokenTypeName { TokenTypeName $$ }

%%

-- If performance becomes a concern will need to parse sequences another way
-- (see Happy docs)

file : fileContents { $1 }

fileContents
  : {- none -}       { [] }
  | function fileContents { $1 : $2 }
  | eol fileContents  { $2 }


type
  : "~" "^" type     { TempRef Mutable $3 }
  | "~" "&" type     { PersRef Mutable $3 }
  | "~" "?" type     { Option Mutable $3}
  | "~"     dataType { DataType Mutable $2 }
  | "~"     "$"      { DataType Mutable TypeInferred }
  |     "^" type     { TempRef Immutable $2 }
  |     "&" type     { PersRef Immutable $2 }
  |     "?" type     { Option Immutable $2 }
  |     "*" type     { ZeroOrMore $2 }
  |     "+" type     { OneOrMore $2 }
  |         dataType { DataType Immutable $1 }
  |         "$"      { DataType Immutable TypeInferred }

dataType
  : Bln { TypeBln }
  | Chr { TypeChr }
  | Flt { TypeFlt }
  | Int { TypeInt }
  | Nat { TypeNat }
  | Str { TypeStr }
  -- Should None be a type?
  | typeName { TypeUser $1 }

typeName
  : tokenTypeName { TypeName $1 }

indentedBlock
  : ind block ded { $2 }

block : stmts { Block $1 }

stmts
  : {- none -}      { [] }
  | stmt            { [$1] }
  | stmt eol stmts  { $1 : $3 }
  | stmt ";" stmts  { $1 : $3 }

stmt
  : lexpr "=" expr  { StmtAssign $1 $3 }
  | variable        { StmtVariable $1 }
  -- | function        { StmtFunction $1 } -- causing some conflicts 
  | ifChain         { StmtIf $1 }
  | apply           { StmtApply $1 }

lexpr
  : apply           { LexprApply $1 }
  | access          { LexprAccess $1 }
  | name            { LexprName $1 }

variable
  : type name "=" expr { Variable $1 $2 $4 }

function
  : signature indentedBlock { Function $1 $2 }

signature
  : purity name parameterList "->" type { Signature $1 $2 $3 $5 }

purity
  : {- none -} { Pure }
  | "@"        { Impure }
  | "~""@"     { SideEffecting }

parameterList
  : "(" parametersCS ")" { $2 }

parametersCS
  : {- none -}                  { [] }
  | parameter                   { [$1] }
  | parameter "," parametersCS  { $1 : $3 }

parameter
  : type name {Parameter $1 $2}

exprsCS
  : {- none -}        { [] }
  | expr              { [$1]}
  -- | expr "," exprsCS  { $1 : $3 } -- also causing conficts. Hmm...

expr
  : ifChain   { ExprIf $1 }
  | lambda    { ExprLambda $1 }
  | apply     { ExprApply $1 }
  | access    { ExprAccess $1 }
  | name      { ExprName $1 }
  | lit       { ExprLit $1 }

ifChain
  : if condBlock                    { IfChainIf $2 IfChainNone }
  | if condBlock else ifChain       { IfChainIf $2 $4 }
  | if condBlock else indentedBlock { IfChainIf $2 $ IfChainElse $4 }

condBlock
  : expr indentedBlock  { CondBlock $1 $2 } -- Would be nice to have one-line ifs

lambda
  : parameterList "=>" indentedBlock { Lambda $1 $3 }

apply
  : expr "(" exprsCS ")"  { Apply $1 $3 }

access
  : expr "." name { Access $1 $3 }

name
  : tokenName { Name $1 }

lit
  : litChr {LitChr $1}
  | litFlt {LitFlt $1}
  | litInt {LitInt $1}
  | litStr {LitStr $1}

