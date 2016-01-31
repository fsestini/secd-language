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
stateN :: Integer -> Bool -> String -> (Token, String)
stateN _ _ "" = error "Unexpected end of string"
stateN soFar sign input@(c:l)
    | isDigitChar c = let d = read [c] :: Integer
                      in stateN (soFar * 10 + d) sign l
    | otherwise = (Number((if sign then -1 else 1) * soFar), input)

-- State SC: string recognition
-- sc input current :: token (resto dell'input)
stateSC :: String -> String -> (Token, String)
stateSC _ "" = error "Unexpected end of input"
stateSC current ('"':xs) = (String current, xs)       -- end scanning at "
stateSC current (x:xs) = stateSC (current ++ [x]) xs  -- continue scanning

-- State S: identifier, operator and keyword recognition
-- stateS :: scannedSoFar -> toScan -> (result, remaining string)
stateS :: String -> String -> (Token, String)
stateS _ "" = error "Unexpected end of input"
stateS current input@(x:xs)
    | isIdChar x = stateS (current ++ [x]) xs
    | otherwise = (extractWord current, input)

-- main state of the automaton
-- stateI :: string to scan -> resulting tokens
stateI :: String -> [Token]
stateI "" = error "Unexpected end of input"
stateI "$" = [(Symbol DOLLAR)]
stateI (' ':xs) = stateI xs   -- ignore spaces
stateI input@(x:xs)
    | isSymbol x    = (Symbol (toSymbol x)) : stateI xs
    | x == '~'      = let (token, newInput) = stateN 0 True xs
                      in token : stateI newInput
    | isDigitChar x = let (token, newInput) = stateN (read [x] :: Integer) False xs
                      in token : stateI newInput
    | isAlphaChar x = let (token, newInput) = stateS [x] xs
                      in token : stateI newInput
    | x == '"'      = let (token, newInput) = stateSC "" xs
                      in token : stateI newInput
    | isSpace x     = stateI xs
    | otherwise     = error "Bad string"

lexi :: String -> [Token]
lexi = stateI
