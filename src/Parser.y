{
module Parser where
import Ast
import ParseError
import qualified Tokens as T
}

%name parse
%tokentype { T.Token }
%error { parseError }

%token
  eol           { T.Eol }

  ind           { T.Indent }
  ded           { T.Dedent }

  Bln           { T.TypeBln }
  Chr           { T.TypeChr }
  Flt           { T.TypeFlt }
  Int           { T.TypeInt }
  Nat           { T.TypeNat }
  None          { T.TypeNone }
  Str           { T.TypeStr }
  This          { T.TypeThis }

  if            { T.If }
  else          { T.Else }
  true          { T.True }
  false         { T.False }
  and           { T.And }
  or            { T.Or }
  not           { T.Not }
  none          { T.None }

  tknNamespace  { T.Namespace }
  tknClass      { T.Class }

  pub           { T.Pub }
  pro           { T.Pro }
  pri           { T.Pri }

  "~"           { T.Tilde }
  "@"           { T.At }
  "#"           { T.Hash }
  "$"           { T.Dollar }
  "^"           { T.Caret }
  "&"           { T.Ampersand }
  "*"           { T.Star }
  "("           { T.LParen }
  ")"           { T.RParen }
  "-"           { T.Minus }
  "+"           { T.Plus }
  "="           { T.Equal }
  "["           { T.LBracket }
  "]"           { T.RBracket }
  ";"           { T.Semicolon}
  ":"           { T.Colon }
  ","           { T.Comma }
  "."           { T.Dot }
  "/"           { T.Slash }
  "?"           { T.QMark }

  "<"           { T.Lesser }
  ">"           { T.Greater }
  "<="          { T.LesserEq }
  ">="          { T.GreaterEq}

  "->"          { T.ThinArrow }
  "=>"          { T.FatArrow }

  litChr        { T.LitChr $$ }
  litFlt        { T.LitFlt $$ }
  litInt        { T.LitInt $$ }
  litStr        { T.LitStr $$ }

  name          { T.Name $$ }
  typename      { T.Typename $$ }

%right "+" "-" 
%right "*" "/"
%right prec_neg

%%

-- If performance becomes a concern will need to parse sequences another way
-- (see Happy docs)

root : unitsOrNone { $1 }

indentedUnits
  : {- none -}    { [] }
  | ind units ded { $2 } 

unitsOrNone
  : {- none -}    { [] }
  | units         { $1 }

units
  : unit            { [$1] }
  | unit eol units  { $1 : $3}

unit
  : namespace       { $1 }
  | class           { UClass $1 }
  | function        { UFunc $1 }
  | variable        { UVar $1 }

namespace
  : tknNamespace name indentedUnits { UNamespace $2 $3 }

class
  : tknClass typename indentedMembers  { Class $2 $3 } 

indentedMembers
  : {- none -}      { [] }
  | ind members ded { $2 }

members
  : member              { [$1] }
  | member eol members  { $1 : $3 }

member
  : accessMod class                             { MClass $1 $2 }
  | accessMod mut function                      { MFunc $1 $2 $3 }
  | accessMod This lambda                       { MCons $1 $3 }
  | accessMod variable                          { MVar $1 $2}

accessMod
  : pub   { Pub }  
  | pro   { Pro }
  | pri   { Pri }

function
  : name lambda { Func $1 $2 }

lambda
  : signature "=>" block { Lambda $1 $3 }
 
signature
  : purityAndTypedNames optionRetType { Sig (fst $1) (snd $1) $2}

purityAndTypedNames
  : "(" ")"                       { (Pure, []) }
  | "(" typedNames ")"            { (Pure, $2) }
  | "(" purity ")"                { ($2, []) }
  | "(" purity "," typedNames ")" { ($2, $4) }

typedNames
  : typedName                 { [$1] }
  | typedName "," typedNames  { $1 : $3 }

typedName
  : type name { TypedName $1 $2 }

paramTypeList
  : type      { (Pure, [$1]) }

funcType
  : type                   retType  { TFunc Pure [$1] $2 }
  | purity                 retType  { TFunc $1 [] $2 }
  | "(" purityAndTypes ")" retType  { TFunc (fst $2) (snd $2) $4 }

purityAndTypes
  : {- none -}        { (Pure, []) }
  | types             { (Pure, $1) }
  | purity            { ($1, []) }
  | purity "," types  { ($1, $3) }

optionRetType
  : {- none -}  { TInferred Immutable}
  | retType     { $1 }

retType
  : "->" type   { $2 }

types
  : type            { [$1] }
  | type "," types  { $1 : $3 }

cons
  : typename "(" purityAndExprs ")" { ECons $1 (fst $3) (snd $3) }

apply
  : expr "(" purityAndExprs ")" { EApply $1 (fst $3) (snd $3) }

purityAndExprs
  : {- none -}        { (Pure, []) }
  | exprs             { (Pure, $1) }
  | purity            { ($1, []) }
  | purity "," exprs  { ($1, $3) }

exprs
  : expr            { [$1]}
  | expr "," exprs  { $1 : $3 }

purity
  : "@"     { ReadWorld }
  | "~""@"  { WriteWorld }

type
  : mut typename  { TUser $1 $2 }
  | funcType      { $1 }
  | mut "$"       { TInferred $1 }
  | mut "^" type  { TTempRef $1 $3 }
  | mut "&" type  { TPersRef $1 $3 }
  | mut "?" type  { TOption $1 $3 }
  | "*" type      { TZeroPlus $2 }
  | "+" type      { TOnePlus $2 }

  | mut Bln       { TBln $1 }
  | mut Chr       { TChr $1 }
  | mut Flt       { TFlt $1 }
  | mut Int       { TInt $1 }
  | mut Nat       { TNat $1 }
  | mut Str       { TStr $1 }

  | None          { TNone }

mut
  : {- none -} { Immutable }
  | "~"        { Mutable }

block
  : indentedBlock { $1 }
  | inlineBlock   { $1 }

indentedBlock
  : ind subBlocks ded { $2 }

subBlocks
  : subBlock                { $1 }
  | subBlock eol subBlocks  { $1 ++ $3 }

subBlock
  : inlineBlock { $1 }
  | nestedBlock { [$1] }

inlineBlock
  : stmt                  { [$1] }
  | stmt ";" inlineBlock  { $1 : $3 }

nestedBlock
  : function        { SFunc $1 }
  | ifBranch        { SIf $1 }

stmt
  : lexpr "=" expr  { SAssign $1 $3 }
  | variable        { SVar $1 }
  -- | apply           { SApply $1 }
  | expr            { SExpr $1 }

ifBranch
  : if condBlock                    { Iff $2 }
  | if condBlock else indentedBlock { IfElse $2 $4 }
  | if condBlock else ifBranch      { IfElif $2 $4 }

condBlock
  : expr indentedBlock { CondBlock $1 $2 }

variable
  : typedName "=" expr { Var $1 $3 }

expr
  : expr if shallowExpr else optionEol expr { EIf $1 $3 $6 }
  | lambda      { ELambda $1 }
  | shallowExpr { $1 }

optionEol
  : eol         {}
  | {- none -}  {}

shallowExpr
  : lexpr     { LExpr $1 }
  | cons      { $1 }

  | op        { $1 }

  | litBln { ELitBln $1 }
  | litChr { ELitChr $1 }
  | litFlt { ELitFlt $1 }
  | litInt { ELitInt $1 }
  | litStr { ELitStr $1 }

lexpr
  : apply           { $1 }
  | expr "." name   { ESelect $1 $3 }
  | name            { EName $1 }

op
  : "(" expr ")"            { $2 }
  | "-" expr %prec prec_neg { ENegate $2 }
  | expr "+" expr           { EAdd $1 $3 }
  | expr "-" expr           { ESub $1 $3 }
  | expr "*" expr           { EMul $1 $3 }
  | expr "/" expr           { EDiv $1 $3 }
  | expr ">" expr           { EGreater $1 $3 }
  | expr "<" expr           { ELesser $1 $3 }
  | expr ">=" expr          { EGreaterEq $1 $3 }
  | expr "<=" expr          { ELesserEq $1 $3 } 

litBln
  : true  {True}
  | false {False}

