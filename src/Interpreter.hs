module Interpreter(
  interpreter,
  run,
  RValue(..),
) where

import Compiler
import SyntaxAnalyzer
import Lexer
import Debug.Trace

-- Values that are found in the Stack and Environment registers
data RValue = V LKC                     -- simple value
            | OGA                       -- environment placeholder
            | CLO [Secdexpr] [[RValue]] -- closures
            | ValueList [RValue]        -- list
            deriving(Show,Eq)

-- Values that are found in the Dump register
data DumpValue = CONTR  [Secdexpr]              -- piece of Control register
               | TRIPLA [RValue] [[RValue]] [Secdexpr] -- triple (S E C)
               deriving(Show,Eq)

type Stack = [RValue]
type Environment = [[RValue]]
type Dump = [DumpValue]
type Control = [Secdexpr]

-- funzioni per la ricerca degli R-valori dati i loro indirizzi: usate da Ld
index :: Integer -> [a] -> a
index n s = if n == 0
            then head s
            else index (n-1) (tail s)

-- Locate the value stored in the b-th slot of the a-th activation record
locate :: (Integer, Integer) -> Environment -> RValue
locate (a,b) e = index b (index a e)

extractInt (V (NUM x)) = x
extractInt x = error ("found not a number: " ++ (show x))

-- List operations on ValueList
vhead :: RValue -> RValue
vhead (ValueList (a:b)) = a
vhead (ValueList [])  = error "vhead found empty list"
vhead _ = error "vhead found not ValueList"

vtail :: RValue -> RValue
vtail (ValueList (a:b)) = ValueList b
vtail (ValueList [])  = error "vtail found empty list";
vtail _ = error "vtail found not ValueList"

-- Check if a value is an atom
vatom :: RValue -> RValue
vatom (V k) = V (BOO True)
vatom _ = V (BOO False)

-- Encapsulate a bool in an LKC value
boolAsLkc :: Bool -> LKC
boolAsLkc b = if b then (BOO True) else (BOO False)

-- Equality test between RValues
eqRValue :: RValue -> RValue -> Bool
eqRValue a@(V _) b = (eqV a b)
eqRValue a@(ValueList _) b = (eqVLISTA a b)
eqRValue a b = error ("invalid equality test between closures"
                      ++ (show a) ++ (show b))

-- Equality test between ValueList
eqVLISTA::RValue -> RValue ->Bool
eqVLISTA (ValueList []) (ValueList [])= True
eqVLISTA (ValueList(a:b)) (ValueList (c:d)) =
    (eqRValue a c) && (eqVLISTA (ValueList b) (ValueList d))
eqVLISTA _ _ = False

-- Equality test between atoms
eqV (V a) (V b) = a == b
eqV _ _ = False

-- Main function
interpreter:: Stack -> Environment -> Control -> Dump -> RValue
interpreter s e ((Ld (b, n)):c) d = let x = locate (b,n) e
                                    in interpreter (x:s) e c d
interpreter s e ((Ldc k):c) d =
    case k of
        NIL -> (interpreter ((ValueList []):s) e c d)
        _   -> (interpreter ((V k):s) e c d)
interpreter (x:y:s) e (Add:c) d = let op1 = extractInt x
                                      op2 = extractInt y
                                  in (interpreter ((V (NUM (op1 + op2))):s) e c d)
interpreter (x:y:s) e (Sub:c) d = let op1 = extractInt x
                                      op2 = extractInt y
                                  in (interpreter ((V (NUM (op1 - op2))):s) e c d)
interpreter (x:y:s) e (Mult:c) d = let op1 = extractInt x
                                       op2 = extractInt y
                                   in (interpreter ((V (NUM (op1 * op2))):s) e c d)
interpreter (x:y:s) e (Div:c) d = let op1 = extractInt x
                                      op2 = extractInt y
                                  in (interpreter ((V (NUM (div op1 op2))):s) e c d)
interpreter (x:y:s) e (Rem:c) d = let op1 = extractInt x
                                      op2 = extractInt y
                                  in (interpreter ((V (NUM (mod op1 op2))):s) e c d)
interpreter (x:y:s) e (Leq:c) d = let op1 = extractInt x
                                      op2 = extractInt y
                                  in (interpreter ((V (BOO (op1 <= op2))):s) e c d)
interpreter (x:y:s) e (Eq:c) d = interpreter ((V (BOO (eqRValue x y))):s) e c d
interpreter (l@(ValueList _):s) e (Car:c) d = interpreter ((vhead l):s) e c d
interpreter (l@(ValueList _):s) e (Cdr:c) d = interpreter ((vtail l):s) e c d
interpreter (x:(ValueList l):s) e (Cons:c) d =
    interpreter ((ValueList (x:l)):s) e c d
interpreter (x:s) e (Atom:c) d = interpreter ((vatom x):s) e c d
interpreter (b:s) e ((Sel c1 c2):c) d =
    case b of
        (V (BOO True))  -> (interpreter s e c1 ((CONTR c):d))
        (V (BOO False)) -> (interpreter s e c2 ((CONTR c):d))
interpreter s e (Join:_) ((CONTR c):d) = interpreter s e c d
interpreter s e ((Ldf code):c) d = interpreter ((CLO code e):s) e c d
interpreter (x:_) _ (Rtn:_) ((TRIPLA s e c):d) = interpreter (x:s) e c d
interpreter s e (Push:c) d = interpreter s ([OGA]:e) c d
interpreter (x:s) _ (Stop:_) _ = x
interpreter ((CLO fCode fEnv):(ValueList l):s) e (Ap:c) d =
    interpreter [] (l:fEnv) fCode ((TRIPLA s e c):d)
interpreter ((CLO letrecBody ([OGA]:fEnv)):(ValueList l):s) e (Rap:c) d =
    interpreter [] ((lazyE l l):fEnv) letrecBody ((TRIPLA s (tail e) c):d)
interpreter _ _ _ _ = error "Incoherent state"

lazyE :: [RValue] -> [RValue] -> [RValue]
lazyE [] _ = []
lazyE (a:b) c = (lazyClo a c):(lazyE b c)

lazyClo :: RValue -> [RValue] -> RValue
lazyClo (CLO a ([OGA]:b)) c = (CLO a ((lazyE c c):b))
lazyClo (V x) _ = (V x)
lazyClo (ValueList x) _ = (ValueList x)
lazyClo x _ = error ("LazyClo found incompatible value " ++ (show x))

run :: String -> RValue
run source = let lexed = lexi source
                 (Return (_, lkced)) = prog lexed
                 compiled = compile lkced
             in interpreter [] [] (compiled ++ [Stop]) []
