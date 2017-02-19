module Tokens where

data Token
  = TokenEol

  | TokenTypeBln
  | TokenTypeChr
  | TokenTypeFlt
  | TokenTypeInt
  | TokenTypeNat
  | TokenTypeStr

  | TokenIf
  | TokenElse
  | TokenTrue
  | TokenFalse
  | TokenAnd
  | TokenOr
  | TokenNot
  | TokenNone

  | TokenIndent
  | TokenDedent

  | TokenTilde
  | TokenAt
  | TokenHash
  | TokenDollar
  | TokenCaret
  | TokenAmpersand
  | TokenStar
  | TokenLParen
  | TokenRParen
  | TokenMinus
  | TokenPlus
  | TokenEqual
  | TokenLBracket
  | TokenRBracket
  | TokenSemicolon
  | TokenColon
  | TokenComma
  | TokenDot
  | TokenQMark

  | TokenThinArrow
  | TokenFatArrow

  | TokenLitChr Char
  | TokenLitInt Int
  | TokenLitFlt Float
  | TokenLitStr String

  | TokenName String
  | TokenTypeName String

  deriving (Eq, Show)

