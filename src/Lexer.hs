module Lexer (
    Token(..),
    SymbolType(..),
    OperatorType(..),
    KeywordType(..),
    lexi
) where

import Prelude hiding (EQ)

data KeywordType = LET | IN | END | LETREC | AND | IF | THEN | ELSE
                 | LAMBDA deriving(Show,Eq)
data OperatorType = EQ | LEQ | CAR | CDR | CONS | ATOM deriving(Show,Eq)
data SymbolType = LPAREN | RPAREN | EQUALS | PLUS | MINUS | TIMES | DIVISION
                | COMMA | DOLLAR deriving(Show,Eq)
data Token = Keyword KeywordType | Operator OperatorType | Id String
           | Symbol SymbolType | Number Integer | String String | Bool Bool
           | Nil deriving(Show,Eq)

-- auxiliary functions
elem' x y = y `elem` x

isAlphaChar = elem' (['a'..'z'] ++ ['A'..'Z'])
isDigitChar = elem' ['0'..'9']
isIdChar c = isAlphaChar c || isDigitChar c
isSeparator = elem' "()=$,"
isSpace = elem' [' ', '\n', '\f', '\r', '\t']
isSymbol = elem' "()=+-*/,"

extractWord :: String -> Token
extractWord w = case w of
  "let"       -> Keyword LET
  "in"        -> Keyword IN
  "end"       -> Keyword END
  "letrec"    -> Keyword LETREC
  "and"       -> Keyword AND
  "if"        -> Keyword IF
  "then"      -> Keyword THEN
  "else"      -> Keyword ELSE
  "lambda"    -> Keyword LAMBDA
  "eq"        -> Operator EQ
  "leq"       -> Operator LEQ
  "car"       -> Operator CAR
  "cdr"       -> Operator CDR
  "cons"      -> Operator CONS
  "atom"      -> Operator ATOM
  "true"      -> Bool True
  "false"     -> Bool False
  "nil"       -> Nil
  otherwise   -> Id w

toSymbol :: Char -> SymbolType
toSymbol c = case c of
  '(' -> LPAREN
  ')' -> RPAREN
  '+' -> PLUS
  '-' -> MINUS
  '*' -> TIMES
  '/' -> DIVISION
  '=' -> EQUALS
  ',' -> COMMA

-- State N: numeral recognition
-- n input numero segno :: token (resto dell'input)
n :: String -> Integer -> Bool -> (Token, String)
n "" _ _ = error "Unexpected end of string"
n input@(c:l) num sign
    | isDigitChar c = let d = read [c] :: Integer
                      in n l (num * 10 + d) sign
    | otherwise = (Number((if sign then -1 else 1) * num), input)

-- State SC: string recognition
-- sc input current :: token (resto dell'input)
sc :: String -> String -> (Token, String)
sc "" _ = error "Unexpected end of input"
sc ('"':xs) current = (String current, xs)
sc (x:xs) current = sc xs (current ++ [x])

-- State S: identifier, operator and keyword recognition
s :: String -> String -> (Token, String)
s "" _ = error "Unexpected end of input"
s input@(x:xs) current
    | isIdChar x = s xs (current ++ [x])
    | otherwise = (extractWord current, input)

i :: String -> [Token]
i "" = error "Unexpected end of input"
i "$" = [(Symbol DOLLAR)]
i (' ':xs) = i xs
i input@(x:xs)
    | isSymbol x = (Symbol (toSymbol x)) : i xs
    | x == '~'      = let (token, newInput) = n xs 0 True in token : i newInput
    | isDigitChar x = let (token, newInput) = n xs (read [x] :: Integer) False
                      in token : i newInput
    | isAlphaChar x = let (token, newInput) = s xs [x] in token : i newInput
    | x == '"'      = let (token, newInput) = sc xs "" in token : i newInput
    | isSpace x     = i xs
    | otherwise     = error "Bad string"

lexi :: String -> [Token]
lexi = i
