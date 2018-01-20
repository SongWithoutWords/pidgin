{
module Parser(parse) where

import Ast.A0Parse
import qualified Lexer.Token as T
import Parser.Error
import Parser.Util
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

%right prec_name
%right "<" ">" "<=" ">=" "==" "!="
%right "+" "-" 
%right "*" "/" "%"
%right prec_neg

%%

-- If performance becomes a concern will need to parse sequences another way
-- (see Happy docs)

root : unitsOrNone { $1 }

unitsOrNone
  : {- none -}    { [] }
  | namedUnits    { $1 }

indentedUnits
  : {- none -}         { [] }
  | ind namedUnits ded { $2 } 

namedUnits
  : namedUnit                    { [$1] }
  | namedUnit optLineSep namedUnits { $1 : $3 }

namedUnit
  : namespace       { fmap UNamespace $1 }
  | namedClass      { fmap UClass $1 }
  | namedFunc       { fmap UFunc $1 }
  | namedVar        { fmap UVar $1 }

namespace
  : tknNamespace name indentedUnits { Named $2 $3 }

namedClass
  : tknClass name indentedMembers  { Named $2 $ Class $3 } 

indentedMembers
  : {- none -}      { [] }
  | ind members ded { $2 }

members
  : member              { [$1] }
  | member eol members  { $1 : $3 }

member
  : accessMod namedClass    { fmap (MClass $1) $2 }
  | accessMod mut namedFunc { fmap (MFunc $1 $2) $3 }
  | accessMod This func     { Named "This" $ MCons $1 $3 }
  | accessMod namedVar      { fmap (MVar $1) $2}

accessMod
  : pub   { Pub }  
  | pro   { Pro }
  | pri   { Pri }

namedFunc
  : name func { Named $1 $2 }

  -- Rule is fine as far as parse errors go
  -- | name "[" types "]"func {  }

func
  : signature "=>" block { Func $1 ImplicitRet $3 }
  | signature ":"  block { Func $1 ExplicitRet $3 }

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
  : lineSep       { [] }
  | expr          { [SExpr $1] }
  | ind stmts ded { $2 }

stmts
  : stmt                { [$1] }
  | stmt lineSep stmts  { $1 : $3 }

stmt
  : expr            { SExpr $1 }
  | expr eqExpr     { SAssign $1 $2 }
  | namedVar        { SVar $1 }
  | ret expr        { SRet $2 }
  | ifBranch        { SIf $1 }

ifBranch
  : if condBlock               { If $2 }
  | if condBlock else block    { IfElse $2 $4 }
  | if condBlock else ifBranch { IfElseIf $2 $4 }

condBlock
  : expr ":" block             { CondBlock $1 $3 }

namedVar
  : mut maybeType name eqExpr { Named $3 $ Var $1 $2 $4 }

eqExpr
  : "=" expr { $2 }

exprs
  : expr            { [$1]}
  | expr "," exprs  { $1 : $3 }

expr
  : name                            { EName $1 }
  | expr "." name                   { ESelect $1 $3 }

  | expr "(" ")"                    { EApp $1 Pure [] }
  | expr "(" exprs ")"              { EApp $1 Pure $3 }
  | expr "(" purity ")"             { EApp $1 $3 [] }
  | expr "(" purity "," exprs ")"   { EApp $1 $3 $5 }

  -- EIf is quite bad for shift reduce conflicts (90 with, 63 without)
  | expr if expr else optEol expr { EIf (Cond $3) $1 $6 }

  -- Less bad (76 with, 63 without)
  -- | if expr then expr else expr {}
  -- | if expr ":" expr else expr {}

  | func      { ELambda $1 }

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

