{
module Parser where

import Preface

import Ast
import Ast0Builder
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
  ret           { T.Ret }
  then          { T.Then }

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
  | unit lineSep units  { $1 : $3}

unit
  : namespace       { $1 }
  | class           { UClass $1 }
  | function        { UFuncL $1 }
  | var             { UVar $1 }

namespace
  : tknNamespace name indentedUnits { UNamespaceL $2 $3 }

class
  : tknClass typename indentedMembers  { ClassL $2 $3 } 

indentedMembers
  : {- none -}      { [] }
  | ind members ded { $2 }

members
  : member              { [$1] }
  | member eol members  { $1 : $3 }

member
  : accessMod class         { MClass $1 $2 }
  | accessMod mut function  { MFuncL $1 $2 $3 }
  | accessMod This lambda   { MCons $1 $3 }
  | accessMod var           { MVar $1 $2}

accessMod
  : pub   { Pub }  
  | pro   { Pro }
  | pri   { Pri }

function
  : name lambda { Func $1 $2 }

lambda
  : signature "=>" block    { Lambda $1 ImplicitRet $3 }
  | signature ":"  block    { Lambda $1 ExplicitRet $3 }

signature
  : purityAndParams optionRetType { SigU (fst $1) (snd $1) $2}

purityAndParams
  : "(" ")"                         { Pure & [] }
  | "(" namedParams ")"             { Pure & $2 }
  | "(" purity ")"                  { $2 & [] }
  | "(" purity "," namedParams ")"  { $2 & $4 }

namedParams
  : namedParam                 { [$1] }
  | namedParam "," namedParams { $1 : $3 }

namedParam
  : mut type name { Param $1 $2 $3 }

funcType
  : type                   retType  { TFunc Pure [$1] $2 }
  | purity                 retType  { TFunc $1 [] $ $2 }
  | "(" purityAndTypes ")" retType  { TFunc (fst $2) (snd $2) $4 }

purityAndTypes
  : {- none -}        { Pure & [] }
  | types             { Pure & $1 }
  | purity            { $1 & [] }
  | purity "," types  { $1 & $3 }

optionRetType
  : {- none -}  { Nothing }
  | retType     { Just $1 }

retType
  : "->" type   { $2 }

types
  : type            { [$1] }
  | type "," types  { $1 : $3 }

cons
  : typename "(" args ")" { ECons $1 $3 }

apply
  : expr "(" args ")" { App $1 $3 }

args
  : {- none -}        { Args Pure [] }
  | exprs             { Args Pure $1 }
  | purity            { Args $1 [] }
  | purity "," exprs  { Args $1 $3 }

exprs
  : expr            { [$1]}
  | expr "," exprs  { $1 : $3 }

purity
  : "@"     { PRead }
  | "~""@"  { PWrite }

-- mType
  -- : mut type { $1 & $2 }

maybeType
  : "$"   { Nothing }
  | type  { Just $1 }

type
  : typename  { TUser $1 }
  | funcType  { $1 }
  | "^" mut type { TTempRef $2 $3 }
  | "&" mut type { TPersRef $2 $3 }
  | "?" mut type { TOption $2 $3 }
  | "*" mut type { TZeroPlus $2 $3 }
  | "+" mut type { TOnePlus $2 $3 }

  | Bln       { TBln }
  | Chr       { TChr }
  | Flt       { TFlt }
  | Int       { TInt }
  | Nat       { TNat }
  | Str       { TStr }

  | None      { TNone }

mut
  : {- none -} { Imut }
  | "~"        { Mut }

block
  : ind stmts ded { $2 }
  | shallowStmt   { [$1] }

optStmts
  : {- none -}  { [] }
  | stmts       { $1 }

stmts
  : stmt            { [$1] }
  | stmt lineSep stmts  { $1 : $3 }

stmt
  : shallowStmt     { $1 }
  | function        { SFunc $1 }
  | ifBranch        { SIf $1 }

shallowStmt
  : lexpr "=" expr  { SAssign $1 $3 }
  | var             { SVar $1 }
  | expr            { SExpr $1 }
  | ret expr        { SRet $2 }

ifBranch
  : if condBlock                    { Iff $2 }
  | if condBlock else block         { IfElse $2 $4 }
  | if condBlock else ifBranch      { IfElif $2 $4 }

condBlock
  : expr then block { CondBlock $1 $3 }

var
  : mut maybeType name "=" expr { VarLu $1 $2 $3 $5 }

expr
  : eIf    { ExprU $1 }
  | lambda { ExprU $ ELambda $1 }

  | apply  { ExprU $ EApp $1 }
  | select { ExprU $ ESelect $1 }
  | name   { ExprU $ EName $1 }
  
  | cons   { ExprU $1 }

  | op     { $1 }

  | litBln { ExprU $ EValBln $1 }
  | litChr { ExprU $ EValChr $1 }
  | litFlt { ExprU $ EValFlt $1 }
  | litInt { ExprU $ EValInt $1 }
  | litStr { ExprU $ EValStr $1 }

eIf
  : expr if expr else optEol expr { EIf $1 $3 $6 }

optEol
  : eol         {}
  | {- none -}  {}

lexpr
  : apply   { LExprU $ LApp $1 }
  | select  { LExprU $ LSelect $1 } 
  | name    { LExprU $ LName $1 }

select
  : expr "." name   { Select $1 $3 }

op
  : "(" expr ")"            { $2 }

  | "-" expr %prec prec_neg { eUnOp Neg $2 } -- ExprU $ ENegate $2 }

  | expr "+" expr           { eBinOp Add $1 $3 } -- ExprU $ EAdd $1 $3 }
  | expr "-" expr           { eBinOp Sub $1 $3 } -- ExprU $ ESub $1 $3 }
  | expr "*" expr           { eBinOp Mul $1 $3 } -- ExprU $ EMul $1 $3 }
  | expr "/" expr           { eBinOp Div $1 $3 } -- ExprU $ EDiv $1 $3 }
  | expr ">" expr           { eBinOp Greater $1 $3 } -- ExprU $ EGreater $1 $3 }
  | expr "<" expr           { eBinOp Lesser $1 $3 } -- ExprU $ ELesser $1 $3 }
  | expr ">=" expr          { eBinOp GreaterEq $1 $3 } -- ExprU $ EGreaterEq $1 $3 }
  | expr "<=" expr          { eBinOp LesserEq $1 $3 } -- ExprU $ ELesserEq $1 $3 } 

  | expr name expr          { eBinOp (OpUser $2) $1 $3 }

litBln
  : true  {True}
  | false {False}

lineSep
  : eol {}
  | ";" {}

