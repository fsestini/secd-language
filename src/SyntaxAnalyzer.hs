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

type Parser a = [Token] -> Exc ([Token], a)

instance Show a => Show (Exc a) where
    show (Raise e)  = "Syntax error: " ++ e
    show (Return x) = "Ok."

instance Functor Exc where
    fmap _ (Raise e)  = Raise e
    fmap f (Return x) = Return (f x)

instance Applicative Exc where
    pure = Return
    (Return f) <*> (Return x) = Return (f x)

instance Monad Exc where
    return           = Return
    (Raise e)  >>= q = Raise e
    (Return x) >>= q = q x

{- Utility match functions -}

matchToken :: Token -> [Token] -> Exc [Token]
matchToken token (t:rest) =
    if t == token then return rest
                  else Raise ("found " ++ (show t)
                              ++ ", expected " ++ (show token))

matchLeftParen :: [Token] -> Exc [Token]
matchLeftParen = matchToken (Symbol LPAREN)

matchRightParen :: [Token] -> Exc [Token]
matchRightParen = matchToken (Symbol RPAREN)

matchBindingsAndBody :: [Token] -> Exc ([Token], [(LKC,LKC)], LKC)
matchBindingsAndBody tokens = do (x, bindingsLkc) <- bind tokens
                                 y <- matchToken (Keyword IN) x
                                 (z, expLkc) <- exp y
                                 w <- matchToken (Keyword END) z
                                 return (w, bindingsLkc, expLkc)

unaryOperator :: (LKC -> LKC) -> Parser LKC
unaryOperator ctor b = do x <- matchLeftParen b
                          (y, param) <- exp x
                          z <- matchRightParen y
                          return (z, ctor param)

binaryOperator :: (LKC -> LKC -> LKC) -> Parser LKC
binaryOperator ctor b = do x <- matchLeftParen b
                           (y, param1) <- exp x
                           z <- matchToken (Symbol COMMA) y
                           (w, param2) <- exp z
                           ww <- matchRightParen w
                           return (ww, ctor param1 param2)

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
bind ((Id a):b) = do x <- matchToken (Symbol EQUALS) b
                     (y, expLkc) <- exp x
                     (z, funXLkc) <- funX y
                     return (z, (VAR a, expLkc):funXLkc)
bind (a:_) = Raise ("found " ++ (show a) ++ ", expected identifier")

funX :: Parser [(LKC, LKC)]
funX ((Keyword AND):tokens)  = bind tokens
funX tokens = Return (tokens, [])

exp :: Parser LKC
exp tokens@((Keyword LET):_)    = prog tokens
exp tokens@((Keyword LETREC):_) = prog tokens
exp ((Keyword LAMBDA):b) = do
        x <- matchLeftParen b
        (y, paramsLkc) <- seqVar x
        z <- matchRightParen y
        (w, expLkc) <- exp z
        return (w, LAMBDAC paramsLkc expLkc)
exp ((Operator CONS):b)         = binaryOperator CONSC b
exp ((Operator LEQ):b)          = binaryOperator LEQC b
exp ((Operator EQ):b)           = binaryOperator EQC b
exp ((Operator CAR):b)          = unaryOperator CARC b
exp ((Operator CDR):b)          = unaryOperator CDRC b
exp ((Operator ATOM):b)         = unaryOperator ATOMC b
exp ((Keyword IF):b)            = do (x, guard) <- exp b
                                     y <- matchToken (Keyword THEN) x
                                     (z, thenBranch) <- exp y
                                     w <- matchToken (Keyword ELSE) z
                                     (k, elseBranch) <- exp w
                                     return (k, IFC guard thenBranch elseBranch)
exp x =  expA x

{- Non-terminal ExpA -}
expA :: Parser LKC
expA a = do (x, tLkc) <- funT a
            e1 tLkc x

{- Non-terminal E1 -}
e1 :: LKC -> Parser LKC
e1 inhLkc ((Symbol PLUS):b)  = do
                                 (x, tLkc) <- funT b
                                 e1 (ADD inhLkc tLkc) x
e1 inhLkc ((Symbol MINUS):b) = do
                                 (x, tLkc) <- funT b
                                 e1 (SUB inhLkc tLkc) x
e1 inhLkc tokens             = Return (tokens, inhLkc) -- epsilon production

{- Non-terminal T -}
funT :: Parser LKC
funT tokens = do (x, fLkc) <- funF tokens
                 (y, t1Lkc) <- funT1 fLkc x
                 return (y, t1Lkc)

{- Non-terminal T1 -}
funT1 :: LKC -> Parser LKC
funT1 inhLkc ((Symbol TIMES):b) = do (x, fLkc) <- funF b
                                     funT1 (MULT inhLkc fLkc) x
funT1 inhLkc ((Symbol DIVISION):b) = do (x, fLkc) <- funF b
                                        funT1 (DIV inhLkc fLkc) x
funT1 inhLkc x = Return (x, inhLkc) -- epsilon production

{- Non-terminal F -}
funF :: Parser LKC
funF ((Id varId):tokens)      = funY varId tokens
funF (Nil:tokens)             = Return (tokens, NIL)
funF ((Number n):tokens)      = Return (tokens, NUM n)
funF ((Bool bb):tokens)       = Return (tokens, BOO bb)
funF ((String str):tokens)    = Return (tokens, STRI str)
funF ((Symbol LPAREN):tokens) = do (x, expALkc) <- expA tokens
                                   y <- matchRightParen x
                                   return (y, expALkc)
funF (a:_) = Raise ("expected expression, found " ++ (show a))

{- Non-terminal Y -}
funY :: String -> Parser LKC
funY varId ((Symbol LPAREN):tokens) = do (x, seq) <- seqExp tokens
                                         y <- matchRightParen x
                                         return (y, CALL (VAR varId) seq)
funY varId x = return (x, VAR varId)  -- epsilon production

expSequence :: Parser [LKC]
expSequence tokens = do (x, expression) <- exp tokens
                        (y, sequence) <- seqExp2 x
                        return (y, expression : sequence)

{- Non-terminal SeqExp -}
seqExp :: Parser [LKC]
seqExp tokens@(Symbol RPAREN : _) = Return (tokens, [])  -- epsilon production
seqExp tokens = expSequence tokens

seqExp2 :: Parser [LKC]
seqExp2 ((Symbol COMMA):tokens) = expSequence tokens
seqExp2 tokens = Return (tokens, [])

seqVar :: Parser [LKC]
seqVar ((Id a):tokens) = do (x, sequence) <- seqVar tokens
                            return (x, (VAR a):sequence)
seqVar x = Return (x, [])
