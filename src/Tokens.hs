module Tokens where

data Token = TokenEol

           | TokenBln
           | TokenChr
           | TokenFlt
           | TokenInt
           | TokenNat
           | TokenStr

           | TokenNone
           | TokenTrue
           | TokenFalse
           | TokenAnd
           | TokenOr
           | TokenNot

           | TokenThinArrow
           | TokenFatArrow

           | TokenIndent
           | TokenDedent

           | TokenLParen
           | TokenRParen
           | TokenLBracket
           | TokenRBracket

           | TokenAt
           | TokenHash
           | TokenCaret
           | TokenAmpersand
           | TokenTilde
           | TokenQMark
           | TokenStar
           | TokenPlus
           | TokenMinus
           | TokenDot

           | TokenIntLit Int
           | TokenFltLit Float
           | TokenStrLit String
           | TokenChrLit Char

           | TokenType String
           | TokenName String

           deriving (Eq, Show)


