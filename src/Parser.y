{
module Parser
  ( parse
  , parseExpr
  , parseType
  ) where

import Ast.A0Parse
import qualified Lexer.Token as T
import Parser.Error
import Parser.Util
}

%name parse unitsOrNone
%name parseExpr expr
%name parseType type

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

  namespace     { T.Namespace }
  data          { T.Data }

  pub           { T.Pub }
  pro           { T.Pro }
  pri           { T.Pri }

  "~"           { T.Tilde }
  "@"           { T.At }
  "#"           { T.Hash }
  "$"           { T.Dollar }
  "%"           { T.Percent }
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
  "=="          { T.EqualEqual }
  "!="          { T.NotEqual }

  "->"          { T.ThinArrow }
  "=>"          { T.FatArrow }

  litChr        { T.LitChr $$ }
  litFlt        { T.LitFlt $$ }
  litInt        { T.LitInt $$ }
  litStr        { T.LitStr $$ }

  name          { T.Name $$ }

%nonassoc "="
%nonassoc name
%nonassoc or
%nonassoc and
%nonassoc "<" ">" "<=" ">=" "==" "!="
%right "+" "-" 
%right "*" "/" "%"
%right prec_neg

%%

-- If performance becomes a concern will need to parse sequences another way
-- (see Happy docs)

unitsOrNone
  : {- none -}    { [] }
  | namedUnits    { $1 }

indentedUnits
  : {- none -}         { [] }
  | ind namedUnits ded { $2 } 

namedUnits
  : namedUnit             { [$1] }
  | namedUnit namedUnits  { $1 : $2 }
  | lineSep namedUnits    { $2 }

namedUnit
  : namespace name indentedUnits  { ($2, UNamespace $3) }
  | namedData                     { fmap UData $1 }
  | namedFunc                     { fmap UFunc $1 }
  | namedVar                      { fmap UVar $1 }

namedData
  : data name indentedMembers  { ($2, $3) } 

indentedMembers
  : {- none -}      { [] }
  | ind members ded { $2 }

members
  : member              { [$1] }
  | member eol members  { $1 : $3 }

member
  : accessMod namedData    { fmap (MData $1) $2 }
  | accessMod type name    { ($3, MVar $1 $2) }

accessMod
  : {- none -}  { Pub }
  | pub         { Pub }  
  | pro         { Pro }
  | pri         { Pri }

namedFunc
  : name func { ($1, $2) }

  -- Rule is fine as far as parse errors go
  -- | name "[" types "]"func {  }

func
  : signature "=>" block { Func $1 $3 }

signature
  : purityAndParams optionRetType { Sig (fst $1) (snd $1) $2}

purityAndParams
  : "(" ")"                         { (Pure, []) }
  | "(" namedParams ")"             { (Pure, $2) }
  | "(" purity ")"                  { ($2, []) }
  | "(" purity "," namedParams ")"  { ($2, $4) }

namedParams
  : namedParam                 { [$1] }
  | namedParam "," namedParams { $1 : $3 }

namedParam
  : mut type name { Param $1 $2 $3 }

optionRetType
  : {- none -}  { Nothing }
  | retType     { Just $1 }

retType
  : "->" type   { $2 }

block
  : expr            { [$1] }
  | ind lsExprs ded { $2 }

namedVar
  : mut maybeType name eqExpr { ($3, Var $1 $2 $4) }

eqExpr
  : "=" expr { $2 }

-- line-separated exprs
lsExprs
  : expr            { [$1]}
  | expr lineSep lsExprs  { $1 : $3 }

-- comma-separated exprs
csExprs
  : expr              { [$1]}
  | expr "," csExprs  { $1 : $3 }

expr
  : name                            { EName $1 }
  | expr "." name                   { ESelect $1 $3 }
  | expr eqExpr                     { EAssign $1 $2 }
  | namedVar                        { EVar $1 }

  | expr "(" ")"                    { EApp $1 Pure [] }
  | expr "(" csExprs ")"              { EApp $1 Pure $3 }
  | expr "(" purity ")"             { EApp $1 $3 [] }
  | expr "(" purity "," csExprs ")"   { EApp $1 $3 $5 }

  -- EIf is quite bad for shift reduce conflicts (90 with, 63 without)
  | if expr then block else optEol block { EIf $2 $4 $7 }
  | if expr then block            { EIf $2 $4 [] }

  | func                    { ELambda $1 }

  | "(" expr ")"            { $2 }

  -- ops
  | "-" expr %prec prec_neg { eUnOp "-" $2 }

  | expr "+" expr           { eBinOp $1 "+" $3 }
  | expr "-" expr           { eBinOp $1 "-" $3 }
  | expr "*" expr           { eBinOp $1 "*" $3 }
  | expr "/" expr           { eBinOp $1 "/" $3 }
  | expr "%" expr           { eBinOp $1 "%" $3 }

  | expr ">" expr           { eBinOp $1 ">" $3 }
  | expr "<" expr           { eBinOp $1 "<" $3 }
  | expr ">=" expr          { eBinOp $1 ">=" $3 }
  | expr "<=" expr          { eBinOp $1 "<=" $3 }
  | expr "==" expr          { eBinOp $1 "==" $3 }
  | expr "!=" expr          { eBinOp $1 "!=" $3 }

  | expr and expr           { eBinOp $1 "and" $3 }
  | expr or expr            { eBinOp $1 "or" $3 }
  | expr name expr          { eBinOp $1 $2 $3 }

  -- Values
  | true   { EVal $ VBln True }
  | false  { EVal $ VBln False }
  | litChr { EVal $ VChr $1 }
  | litFlt { EVal $ VFlt $1 }
  | litInt { EVal $ VInt $1 }
  | litStr { EVal $ VStr $1 }

types
  : type            { [$1] }
  | type "," types  { $1 : $3 }

maybeType
  : "$"   { Nothing }
  | type  { Just $1 }

type
  : name  { TUser $1 }
  | funcType  { $1 }
  | "^" mut type { TRef $2 $3 }
  -- | "&" mut type { TPersRef $2 $3 }
  -- | "?" mut type { TOption $2 $3 }
  -- | "*" mut type { TZeroPlus $2 $3 }
  -- | "+" mut type { TOnePlus $2 $3 }
  | type "[" types "]" { TArgs $3 $1 }

  | Bln       { TBln }
  | Chr       { TChr }
  | Flt       { TFlt }
  | Int       { TInt }
  | Nat       { TNat }
  | Str       { TStr }

  | None      { TNone }

funcType
  : type                   retType  { TFunc Pure [$1] $2 }
  | purity                 retType  { TFunc $1 [] $ $2 }
  | "(" purityAndTypes ")" retType  { TFunc (fst $2) (snd $2) $4 }

purityAndTypes
  : {- none -}        { (Pure, []) }
  | types             { (Pure, $1) }
  | purity            { ($1, []) }
  | purity "," types  { ($1, $3) }

purity
  : "@"     { PRead }
  | "~""@"  { PWrite }

mut
  : {- none -} { Imt }
  | "~"        { Mut }

optLineSep
  : lineSep     {}
  | {- none -}  {}

lineSep
  : eol {}
  | ";" {}

optEol
  : eol         {}
  | {- none -}  {}

