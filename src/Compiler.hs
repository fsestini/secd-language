module Compiler (
compile,
Secdexpr(..),
c,
d
)
where

import Lexer
import SyntaxAnalyzer

data Secdexpr = Add | Sub |  Mult | Div | Rem | Eq | Leq | Car | Cdr | Cons
              | Atom | Join | Rtn | Stop | Push | Ap | Rap
              | Ld (Integer, Integer) | Ldc LKC | Sel [Secdexpr] [Secdexpr]
              | Ldf [Secdexpr] deriving(Show, Eq)

-- funzioni per il calcolo dell'indirizzo di una variabile nell'ambiente
positionInAr :: String -> [LKC] -> Integer
positionInAr _ [] = error "positionInAr: variable not found"
positionInAr x ((VAR z):y) = if z == x then 0 else 1 + (positionInAr x y)
positionInAr _ _ = error "positionInAr: non-VAR element found"

-- Checks if a variable is defined in the specified activation record
arContainsVar :: String -> [LKC] -> Bool
arContainsVar _ [] = False
arContainsVar x ((VAR z):y) = (x == z) || arContainsVar x y
arContainsVar x _ = error ("found AR with non-VAR element " ++ x)

-- Returns (AR, offset) of a variable
location :: String -> [[LKC]] -> (Integer, Integer)
location x l =
    let searchLocation _ _ [] = error ("location non trova VAR " ++ x)
        searchLocation x count (n:m) = if arContainsVar x n
                                       then (count, positionInAr x n)
                                       else searchLocation x (count + 1) m
    in searchLocation x 0 l

-- Extracts the variables of a list of bindings
vars :: [(a,b)] -> [a]
vars [] = []
vars ((x,y):r) = x:(vars r)

-- Extracts the expressions of a list of bindings
exprs :: [(a,b)] -> [b]
exprs [] = []
exprs((x,y):r) = y:(exprs r)

complist:: [LKC]-> [[LKC]] -> [Secdexpr]->[Secdexpr]
complist [] _ c = (Ldc NIL):c
complist (x:y) n c = complist y n (comp x n (Cons:c))

comp:: LKC -> [[LKC]] -> [Secdexpr]->[Secdexpr]
comp e n c = case e of
    (VAR x)                 -> ((Ld (location x n)):c)
    (NUM x)                 -> (Ldc (NUM x)):c
    (BOO x)                 -> (Ldc (BOO x)):c
    (STRI x)                -> (Ldc (STRI x)):c
    NIL                     -> (Ldc NIL):c
    (ADD x y)               -> comp y n (comp x n (Add:c))
    (SUB x y)               -> comp y n (comp x n (Sub:c))
    (MULT x y)              -> comp y n (comp x n (Mult:c))
    (DIV x y)               -> comp y n (comp x n (Div:c))
    (REM x y)               -> comp y n (comp x n (Rem:c))
    (EQC x y)               -> comp y n (comp x n (Eq:c))
    (LEQC x y)              -> comp y n (comp x n (Leq:c))
    (CARC x)                -> comp x n (Car:c)
    (CDRC x)                -> comp x n (Cdr:c)
    (CONSC x y)             -> comp y n (comp x n (Cons:c))
    (ATOMC x)               -> comp x n (Atom:c)
    (IFC x y z)             -> let thenp = comp y n [Join]
                                   elsep = comp z n [Join]
                               in comp x n ((Sel thenp elsep):c)
    (LAMBDAC par body)      -> (Ldf (comp body (par:n) [Rtn])):c
    (LETC body bindings)    -> complist (exprs bindings) n
                                    ((Ldf (comp body ((vars bindings):n) [Rtn])):(Ap:c))
    (LETRECC body bindings) -> complist (exprs bindings) ((vars bindings):n)
                                    (Push:(Ldf (comp body ((vars bindings):n) [Rtn])):(Rap:c))
    (CALL id args)  -> complist args n (comp id n (Ap:c))
    _               -> []

--esempi di prova

c="letrec  FACT = lambda ( X ) if  eq ( X, 0 ) then 1 else  X*FACT(  X - 1 ) and G = lambda ( H L ) if  eq ( nil, L ) then L else cons( H(car( L ) ), G ( H, cdr ( L ) )) in G ( FACT, cons(1 ,cons(2, cons(3, nil))) ) end $"

d= "let x= 4+1 and y= 6 in x*3 + y * 2* x + x*y end $"
simple = "let x = 1 and y = 2 in x + y end $"
lexed = lexi simple
(Return (_, lkced)) = prog lexed

compile x = comp x [] []
