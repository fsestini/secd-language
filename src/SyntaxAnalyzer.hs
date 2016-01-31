module SyntaxAnalyzer (prog, parse, Exc(..), LKC(..)) where

import Lexer
import Prelude hiding (EQ,exp)
import Control.Monad

-- Datatype for the abstract syntax tree
data LKC = ETY --segnala epsilon productions
         | VAR String | NUM Integer | STRI String | BOO Bool | NIL
         | ADD LKC LKC | SUB LKC LKC | MULT LKC LKC | REM LKC LKC| DIV LKC LKC
         | EQC LKC LKC | LEQC LKC LKC | CARC LKC | CDRC LKC | CONSC LKC LKC
         | ATOMC LKC
         | IFC LKC LKC LKC          -- IFC (guard) (true-branch) (false-branch)
         | LAMBDAC [LKC] LKC        -- LAMBDAC (parameters) (body)
         | CALL LKC [LKC]           -- CALL (function id) (arguments)
         | LETC LKC [(LKC,LKC)]     -- LETC (body) (list of bindings)
         | LETRECC LKC [(LKC, LKC)] -- LETRECC (body) (list of bindings)
         deriving(Show, Eq)

type Exception = String
data Exc a = Raise Exception | Return a

instance Show a => Show (Exc a) where
    show (Raise e)= "Syntax error: " ++ e
    show (Return x) = "Ok."

instance Functor Exc where
    fmap _ (Raise e) = Raise e
    fmap f (Return x) = Return (f x)

instance Applicative Exc where
    pure = Return
    (Return f) <*> (Return x) = Return (f x)

instance Monad Exc where
    return            = Return
    (Raise e)  >>= q  = Raise e
    (Return x) >>= q  = q x

{- Utility match functions -}
matchLet :: [Token] -> Exc [Token]
matchLet ((Keyword LET):b)    = Return b
matchLet ((Keyword LETREC):b) = Return b

matchIn :: [Token] -> Exc [Token]
matchIn ((Keyword IN):b) = Return b
matchIn (a:_)            = Raise ("found " ++ (show a) ++ ", expected `IN'")

matchEnd :: [Token] -> Exc [Token]
matchEnd ((Keyword END):b) = Return b
matchEnd (a:_)             = Raise ("found " ++ (show a) ++ ", expected END")

matchThen :: [Token] -> Exc [Token]
matchThen ((Keyword THEN):b) = Return b
matchThen (a:b)              = Raise ("found " ++ (show a) ++ ", expected THEN")

matchElse :: [Token] -> Exc [Token]
matchElse ((Keyword ELSE):b) = Return b
matchElse (a:b)              = Raise ("found " ++ (show a) ++ ", expected ELSE")

matchLeftParen :: [Token] -> Exc [Token]
matchLeftParen ((Symbol LPAREN):b) = Return b
matchLeftParen (a:b)               = Raise ("found " ++ (show a) ++ ", expected (")

matchRightParen :: [Token] -> Exc [Token]
matchRightParen ((Symbol RPAREN):b) = Return b
matchRightParen (a:b)               = Raise ("found " ++ (show a) ++ ", expected )")

matchComma :: [Token] -> Exc [Token]
matchComma ((Symbol COMMA):b) = Return  b
matchComma (a:b)                = Raise ("found " ++ (show a) ++ ", expected ,")

matchEquals :: [Token] -> Exc [Token]
matchEquals ((Symbol EQUALS):b) = Return b
matchEquals (a:b)               = Raise ("found " ++ (show a) ++ ", expected =")

matchExpSequence :: [Token] -> Exc ([Token], [LKC])
matchExpSequence tokens = do
                            x <- matchLeftParen tokens
                            (y, lkc) <- seqExp x
                            z <- matchRightParen y
                            return (z, lkc)

matchBindingsAndBody :: [Token] -> Exc ([Token], [(LKC,LKC)], LKC)
matchBindingsAndBody tokens = do (x, bindingsLkc) <- bind tokens
                                 y <- matchIn x
                                 (z, expLkc) <- exp y
                                 w <- matchEnd z
                                 return (w, bindingsLkc, expLkc)

{- Parser.
 - Translates a tokenized program into its representation as an
 - abstract syntax tree -}
parse :: [Token] -> LKC
parse x = case prog x of
            Raise e -> error e
            Return (_, lkc) -> lkc

{- Productions of the grammar -}

{- Prog production -}
prog :: [Token] -> Exc ([Token], LKC)
prog ((Keyword LET):tokens) = do
        (w, bindings, body) <- matchBindingsAndBody tokens
        return (w, LETC body bindings)
prog ((Keyword LETREC):tokens) = do
        (w, bindings, body) <- matchBindingsAndBody tokens
        return (w, LETRECC body bindings)
prog (a:_) = Raise ("found " ++ (show a) ++ ", expected `LET' or `LETREC'")

{- Bind production -}
bind :: [Token] -> Exc ([Token], [(LKC,LKC)])
bind ((Id a):b) = do
        x <- matchEquals b
        (y, expLkc) <- exp x
        (z, funXLkc) <- funX y
        return (z, (VAR a, expLkc):funXLkc)
bind (a:_) = Raise ("found " ++ (show a) ++ ", expected identifier")

funX :: [Token] -> Exc ([Token], [(LKC,LKC)])
funX ((Keyword AND):b)  = bind b
funX a@((Keyword IN):b) = Return (a, [])
funX (a:_)              = Raise ("found " ++ (show a) ++ " after binders")

exp :: [Token] -> Exc ([Token], LKC)
exp tokens@((Keyword LET):_)    = prog tokens
exp tokens@((Keyword LETREC):_) = prog tokens
exp ((Keyword LAMBDA):b) = do
        x <- matchLeftParen b
        (y, paramsLkc) <- seqVar x
        z <- matchRightParen y
        (w, expLkc) <- exp z
        return (w, LAMBDAC paramsLkc expLkc)
exp ((Operator CONS):b)  = do
        (z, seqExpLkc) <- matchExpSequence b
        if length seqExpLkc == 2
            then return (z, CONSC (head seqExpLkc) (last seqExpLkc))
            else Raise ("found " ++ (show . length) seqExpLkc
                ++ " arguments to CONS, expected 2")
exp ((Operator LEQ):b) = do
        (z, seqExpLkc) <- matchExpSequence b
        if length seqExpLkc == 2
            then return (z, LEQC (head seqExpLkc) (last seqExpLkc))
            else Raise ("found " ++ (show . length) seqExpLkc
                 ++ " arguments to LEQ, expected 2")
exp ((Operator EQ):b) = do
        (z, seqExpLkc) <- matchExpSequence b
        if length seqExpLkc == 2
            then return (z, EQC (head seqExpLkc) (last seqExpLkc))
            else Raise ("found " ++ (show . length) seqExpLkc
                 ++ " arguments to EQ, expected 2")
exp ((Operator CAR):b) = do
        (z, seqExpLkc) <- matchExpSequence b
        if length seqExpLkc == 1
            then return (z, CARC (head seqExpLkc))
            else Raise ("found " ++ (show . length) seqExpLkc
                 ++ " arguments to CAR, expected 1")
exp ((Operator CDR):b) = do
        (z, seqExpLkc) <- matchExpSequence b
        if length seqExpLkc == 1
            then return (z, CDRC (head seqExpLkc))
            else Raise ("found " ++ (show . length) seqExpLkc
                 ++ " arguments to CDR, expected 1")
exp ((Operator ATOM):b) = do
        x <- matchLeftParen b
        (y, seqExpLkc) <- seqExp x
        z <- matchRightParen y
        if length seqExpLkc == 1
            then return (z, ATOMC (head seqExpLkc))
            else Raise ("found " ++ (show . length) seqExpLkc
                 ++ " arguments to ATOM, expected 1")
exp ((Keyword IF):b) = do
        (x, guardLkc) <- exp b
        y <- matchThen x
        (z, thenLkc) <- exp y
        w <- matchElse z
        (ww, elseLkc) <- exp w
        return (ww, IFC guardLkc thenLkc elseLkc)
exp x =  expA x

expA :: [Token] -> Exc ([Token], LKC)
expA a = do
         (x, tLkc) <- funT a
         e1 x tLkc

{- Non-terminal E1 -}
e1 :: [Token] -> LKC -> Exc ([Token], LKC)
e1 ((Symbol PLUS):b) inhLkc  = do
                                 (x, tLkc) <- funT b
                                 e1 x (ADD inhLkc tLkc)
e1 ((Symbol MINUS):b) inhLkc = do
                                 (x, tLkc) <- funT b
                                 e1 x (SUB inhLkc tLkc)
e1 tokens inhLkc             = Return (tokens, inhLkc) -- epsilon production

{- Non-terminal T -}
funT :: [Token] -> Exc ([Token], LKC)
funT a = do
         (x, fLkc) <- funF a
         (y, t1Lkc) <- funT1 x fLkc
         return (y, t1Lkc)

{- Non-terminal T1 -}
funT1 :: [Token] -> LKC -> Exc ([Token], LKC)
funT1 ((Symbol TIMES):b) inhLkc    = do
                                       (x, fLkc) <- funF b
                                       funT1 x (MULT inhLkc fLkc)
funT1 ((Symbol DIVISION):b) inhLkc = do
                                       (x, fLkc) <- funF b
                                       funT1 x (DIV inhLkc fLkc)
funT1 x inhLkc                     = Return (x, inhLkc) -- epsilon production

{- Non-terminal F -}
funF :: [Token] -> Exc ([Token], LKC)
funF ((Id a):b) = funY b a
funF (Nil:b) = Return (b, NIL)
funF ((Number n):b) = Return (b, NUM n)
funF ((Bool bb):b) = Return (b, BOO bb)
funF ((String str):b) = Return (b, STRI str)
funF ((Symbol LPAREN):b) = do
                             (x, expALkc) <- expA b
                             y <- matchRightParen x
                             return (y, expALkc)
funF (a:_) = Raise ("expected expression, found " ++ (show a))

{- Non-terminal Y -}
funY :: [Token] -> String -> Exc ([Token], LKC)
funY ((Symbol LPAREN):b) varId = do
                                  (x, seqExpLkc) <- seqExp b
                                  y <- matchRightParen x
                                  return (y, CALL (VAR varId) seqExpLkc)
funY x varId = return (x, VAR varId)

{- Non-terminal SeqExp -}
seqExp :: [Token] -> Exc ([Token], [LKC])
seqExp a@((Symbol RPAREN):b) = Return (a, [])  -- epsilon production
seqExp a = do
             (x, expLkc) <- exp a
             (y, seqLkc) <- seqExp2 x
             return (y, expLkc : seqLkc)

seqExp2 :: [Token] -> Exc ([Token], [LKC])
seqExp2 ((Symbol COMMA):b) = do (x, expLkc) <- exp b
                                (y, seqExp2Lkc) <- seqExp2 x
                                return (y, expLkc : seqExp2Lkc)
seqExp2 a = Return (a, [])

seqVar :: [Token] -> Exc ([Token], [LKC])
seqVar ((Id a):b) = do
                      (x, seqLkc) <- seqVar b
                      return (x, (VAR a):seqLkc)
seqVar x = Return (x, [])
