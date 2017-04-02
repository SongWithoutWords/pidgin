module Tokens where

data Token
  = TknEol

  | TknIndent
  | TknDedent

  | TknTypeBln
  | TknTypeChr
  | TknTypeFlt
  | TknTypeInt
  | TknTypeNat
  | TknTypeStr
  | TknTypeThis

  | TknIf
  | TknElse
  | TknTrue
  | TknFalse
  | TknAnd
  | TknOr
  | TknNot
  | TknNone

  | TknNamespace
  | TknClass

  | TknPub
  | TknPro
  | TknPri

  | TknTilde
  | TknAt
  | TknHash
  | TknDollar
  | TknCaret
  | TknAmpersand
  | TknStar
  | TknLParen
  | TknRParen
  | TknMinus
  | TknPlus
  | TknEqual
  | TknLBracket
  | TknRBracket
  | TknSemicolon
  | TknColon
  | TknComma
  | TknDot
  | TknQMark

  | TknLess
  | TknGreater
  | TknLessOrEqual
  | TknGreaterOrEqual

  | TknThinArrow
  | TknFatArrow

  | TknLitChr Char
  | TknLitInt Int
  | TknLitFlt Float
  | TknLitStr String

  | TknName String
  | TknTypename String

  deriving (Eq, Show)

