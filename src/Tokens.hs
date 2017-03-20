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

  | TknIf
  | TknElse
  | TknTrue
  | TknFalse
  | TknAnd
  | TknOr
  | TknNot
  | TknNone

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

  | TknThinArrow
  | TknFatArrow

  | TknLitChr Char
  | TknLitInt Int
  | TknLitFlt Float
  | TknLitStr String

  | TknName String
  | TknTypename String

  deriving (Eq, Show)

