{
module Parser where
import Ast
import ParseError
import Tokens
}

%name parse
%tokentype { Token }
%error { parseError }

%token
  eol           { TknEol }

  ind           { TknIndent }
  ded           { TknDedent }

  Bln           { TknTypeBln }
  Chr           { TknTypeChr }
  Flt           { TknTypeFlt }
  Int           { TknTypeInt }
  Nat           { TknTypeNat }
  Str           { TknTypeStr }

  if            { TknIf }
  else          { TknElse }
  true          { TknTrue }
  false         { TknFalse }
  and           { TknAnd }
  or            { TknOr }
  not           { TknNot }
  none          { TknNone }

  tknNamespace  { TknNamespace }
  tknClass      { TknClass }

  pub           { TknPub }
  pro           { TknPro }
  pri           { TknPri }

  "~"           { TknTilde }
  "@"           { TknAt }
  "#"           { TknHash }
  "$"           { TknDollar }
  "^"           { TknCaret }
  "&"           { TknAmpersand }
  "*"           { TknStar }
  "("           { TknLParen }
  ")"           { TknRParen }
  "-"           { TknMinus }
  "+"           { TknPlus }
  "="           { TknEqual }
  "["           { TknLBracket }
  "]"           { TknRBracket }
  ";"           { TknSemicolon}
  ":"           { TknColon }
  ","           { TknComma }
  "."           { TknDot }
  "?"           { TknQMark }

  "->"          { TknThinArrow }
  "=>"          { TknFatArrow }

  litChr        { TknLitChr $$ }
  litFlt        { TknLitFlt $$ }
  litInt        { TknLitInt $$ }
  litStr        { TknLitStr $$ }

  name     { TknName $$ }
  typename { TknTypename $$ }

%%

-- If performance becomes a concern will need to parse sequences another way
-- (see Happy docs)

root : units { $1 }

indentedUnits
  : {- none -}    { [] }
  | ind units ded { $2 } 

units
  : unit            { [$1] }
  | unit eol units  { $1 : $3}

unit
  : namespace       { $1 }
  | class           { UnitClass $1 }
  | function        { UnitFunction $1 }
  | variable        { UnitVariable $1 }

namespace
  : tknNamespace name indentedUnits { UnitNamespace $2 $3 }

class
  : tknClass typename indentedMembers  { Class $2 $3 } 

indentedMembers
  : {- none -}      { [] }
  | ind members ded { $2 }

members
  : member              { [$1] }
  | member eol members  { $1 : $3 }

member
  : accessMod class               { MemberClass $1 $2 }
  | accessMod mutability function { MemberFunction $1 $2 $3 }
  | accessMod variable            { MemberVariable $1 $2}

accessMod
  : pub   { Pub }  
  | pro   { Pro }
  | pri   { Pri }

paramTypes
  : "("")"          { [] } -- '()' must be implemented differently than for parameters/expressions to avoid reduce/reduce conflict
  | "(" typesCS ")" { $2 }

typesCS
  : type              { [$1] }
  | type "," typesCS  { $1 : $3 }

type
  : mutability typename       { TypeUser $1 $2 }
  | purity paramTypes "->" type { TypeFunction $ FunctionType $1 $2 $4 }
  | mutability "$"            { TypeInferred $1 }
  | mutability "^" type       { TypeTempRef $1 $3 }
  | mutability "&" type       { TypePersRef $1 $3 }
  | mutability "?" type       { TypeOption $1 $3 }
  | "*" type                  { TypeZeroPlus $2 }
  | "+" type                  { TypeOnePlus $2 }
  | mutability prim           { TypePrim $1 $2 }

prim
  : Bln { PrimBln }
  | Chr { PrimChr }
  | Flt { PrimFlt }
  | Int { PrimInt }
  | Nat { PrimNat }
  | Str { PrimStr } 

mutability
  : {- none -} { Immutable }
  | "~"        { Mutable }

indentedBlock
  : ind block ded { $2 }

block : stmts { $1 }

stmts
  : {- none -}      { [] }
  | stmt            { [$1] }
  | stmt eol stmts  { $1 : $3 }
  | stmt ";" stmts  { $1 : $3 }

stmt
  : lexpr "=" expr  { StmtAssign $1 $3 }
  | variable        { StmtVariable $1 }
  | function        { StmtFunction $1 } -- causing some conflicts 
  | ifChain         { StmtIf $1 }
  | apply           { StmtApply $1 }

lexpr
  : apply           { LexprApply $1 }
  | access          { LexprAccess $1 }
  | name            { LexprName $1 }

variable
  : typedName "=" expr { Variable $1 $3 }

function
  : signature indentedBlock { Function $1 $2 }

signature
  : purity name parameterList "->" type { Signature $2 $ AnonSig $1 $3 $5 }

purity
  : {- none -} { Pure }
  | "@"        { Impure }
  | "~""@"     { SideEffecting }


parameterList
  : "(" parametersCS ")"  { $2 }

parametersCS
  : {- none -}                  { [] }
  | typedName                   { [$1] }
  | typedName "," parametersCS  { $1 : $3 }

typedName
  : type name { TypedName $1 $2 }

exprsCS
  : {- none -}        { [] }
  | expr              { [$1]}
  | expr "," exprsCS  { $1 : $3 } -- also causing conficts. Hmm...

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
  : purity parameterList optionRet "=>" indentedBlock { Lambda (AnonSig $1 $2 $3) $5 }

optionRet
  : {- none -} { TypeInferred Mutable }
  | "->" type  { $2 }

apply
  : expr "(" exprsCS ")"  { Apply $1 $3 }

access
  : expr "." name { Access $1 $3 }

lit
  : litChr {LitChr $1}
  | litFlt {LitFlt $1}
  | litInt {LitInt $1}
  | litStr {LitStr $1}

