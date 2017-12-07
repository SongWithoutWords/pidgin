module Lexer.FormatTokens(formatTokens) where

import Lexer.Token as T

-- Because we have tokens called True and False
true :: Bool
true = Prelude.True
false :: Bool
false = Prelude.False

formatTokens :: Tokens -> String
formatTokens tokens = formatTokens' tokens 0 false
  where
    formatTokens' :: Tokens -> Int -> Bool -> String
    formatTokens' [] _ _ = ""
    formatTokens' (t:ts) indentPrev separationPrev =
      let
        indentCur = indentPrev + indentation t
        -- separationCur :: Bool
        separationCur = needsSpace t
        tokenStr = formatToken t indentCur
        separation = if separationPrev && separationCur then " " else ""
      in
        separation ++ tokenStr ++ formatTokens' ts indentCur separationCur

indentation :: Token -> Int
indentation t = case t of
  Indent -> 1
  Dedent -> (-1)
  _ -> 0

needsSpace :: Token -> Bool
needsSpace t = case t of
  TypeBln -> true
  TypeChr -> true
  TypeFlt -> true
  TypeInt -> true
  TypeNat -> true
  TypeNone -> true
  TypeStr -> true
  TypeThis -> true

  If -> true
  Then -> true
  Else -> true
  T.True -> true
  T.False -> true
  And -> true
  Or -> true
  Not -> true
  None -> true
  Ret -> true

  Namespace -> true
  Class -> true

  Pub -> true
  Pro -> true
  Pri -> true

  LitChr _ -> true
  LitInt _ -> true
  LitFlt _ -> true
  LitStr _ -> true

  Name _ -> true
  Typename _ -> true

  _ -> false

formatToken :: Token -> Int -> String
formatToken token indent =
  let lineSep = "\n" ++ replicate (4 * indent) ' '
  in case token of
    Eol -> lineSep
    Indent -> lineSep
    Dedent -> lineSep

    TypeBln -> "Bln "
    TypeChr -> "Chr "
    TypeFlt -> "Flt "
    TypeInt -> "Int "
    TypeNat -> "Nat "
    TypeNone -> "None "
    TypeStr -> "Str "
    TypeThis -> "This "

    If -> "if "
    Then -> "then "
    Else -> "else "
    T.True -> "true"
    T.False -> "false"
    And -> "and"
    Or -> "or"
    Not -> "not"
    None -> "none"
    Ret -> "ret"

    Namespace -> "namespace"
    Class -> "class"

    Pub -> "pub"
    Pro -> "pro"
    Pri -> "pri"

    Tilde -> "~"
    At -> "@"
    Hash -> "#"
    Dollar -> "$"
    Percent -> "%"
    Caret -> "^"
    Ampersand -> "&"
    Star -> "*"
    LParen -> "("
    RParen -> ")"
    Minus -> "-"
    Plus -> "+"
    Equal -> "="
    LBracket -> "["
    RBracket -> "]"
    Semicolon -> ";"
    Colon -> ":"
    Comma -> ", "
    Dot -> "."
    QMark -> "?"
    Slash -> "/"

    Lesser -> "<"
    Greater -> ">"
    LesserEq -> "<="
    GreaterEq -> ">="
    EqualEqual -> "=="
    NotEqual -> "!="

    ThinArrow -> "->"
    FatArrow -> "=>"

    LitChr c -> '\'' : c : "\'"
    LitInt i -> show i
    LitFlt f -> show f
    LitStr s -> "\"" ++ s ++  "\""

    Name n -> n
    Typename t -> t

