module Tokens where

type Tokens = [Token]

data Token
  = Eol

  | Indent
  | Dedent

  | TypeBln
  | TypeChr
  | TypeFlt
  | TypeInt
  | TypeNat
  | TypeNone
  | TypeStr
  | TypeThis

  | If
  | Then
  | Else
  | True
  | False
  | And
  | Or
  | Not
  | None
  | Ret

  | Namespace
  | Class

  | Pub
  | Pro
  | Pri

  | Tilde
  | At
  | Hash
  | Dollar
  | Percent
  | Caret
  | Ampersand
  | Star
  | LParen
  | RParen
  | Minus
  | Plus
  | Equal
  | LBracket
  | RBracket
  | Semicolon
  | Colon
  | Comma
  | Dot
  | QMark
  | Slash

  | Lesser
  | Greater
  | LesserEq
  | GreaterEq
  | EqualEqual
  | NotEqual

  | ThinArrow
  | FatArrow

  | LitChr Char
  | LitInt Int
  | LitFlt Float
  | LitStr String

  | Name String
  | Typename String

  deriving (Eq, Show)

