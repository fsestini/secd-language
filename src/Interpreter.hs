--INTERPRETE SECD COMPLETO in Haskell
module Interpreter(
interpreter,
RValue(..),
) where

import Compiler
import SyntaxAnalyzer
import Lexer
import Debug.Trace

-- tipo che modella gli R-valori delle variabili. Si tratta dei valori da
-- mettere nella pila S e nell'ambiente dinamico E.
data RValue = V LKC
            | OGA -- environment placeholder
            | CLO [Secdexpr] [[RValue]] -- closures
            | ValueList [RValue] -- list of values
            deriving(Show,Eq)

-- datatype dei valori del Dump
data DumpValue = CONTR  [Secdexpr]
               | TRIPLA [RValue] [[RValue]] [Secdexpr]
               | DUMMY deriving(Show,Eq)

type Stack = [RValue]
type ActivationRecord = [RValue]
type Environment = [ActivationRecord]
type Dump = [DumpValue]

-- funzione che crea l'ambiente dinamico ricorsivo necessario per il
-- trattamento della ricorsione. Serve nel caso Rap
lazyE :: [RValue] -> [RValue] -> [RValue]
lazyE [] _ = []
lazyE (a:b) c = (lazyClo a c):(lazyE b c)

lazyClo :: RValue -> [RValue] -> RValue
lazyClo (CLO a b) c = (CLO a ((lazyE c c):b))
lazyClo (V x) _ = (V x)
lazyClo (ValueList x) _ = (ValueList x)
lazyClo x _ = error ("LazyClo trova valore incompatibile" ++ (show x))

-- funzioni per la ricerca degli R-valori dati i loro indirizzi: usate da Ld
index :: Integer -> [a] -> a
index n s = if n == 0
            then head s
            else index (n-1) (tail s)

locate :: (Integer, Integer) -> Environment -> RValue
locate (a,b) e = index b (index a e)

extractInt (V (NUM x)) = x
extractInt x = error ("trovato altro da intero" ++ (show x))

-- funzioni per le liste di Valori ValueList
vhead (ValueList (a:b)) = a
vhead (ValueList [])  = error "vhead trovata lista vuota"
vhead _ = error "vhead non trova ValueList"

vtail (ValueList (a:b)) = ValueList b
vtail (ValueList [])  = error "vtail trovata lista vuota";
vtail _ = error "vtail non trova ValueList"

vatom (V k)= V (BOO True)
vatom _ = V (BOO False)

boolAsLkc :: Bool -> LKC
boolAsLkc b = if b then (BOO True) else (BOO False)

-- test di uguaglianza per il tipo RValue, si adatta ai tipi dei parametri con
-- cui é invocata
eqRValue :: RValue -> RValue -> Bool
eqRValue a@(V _) b = (eqV a b)
eqRValue a@(ValueList _) b = (eqVLISTA a b)
eqRValue a  b = error ("uguaglianza tra chiusure"++ (show a) ++ (show b))

eqVLISTA::RValue -> RValue ->Bool
eqVLISTA (ValueList []) (ValueList [])= True
eqVLISTA (ValueList(a:b)) (ValueList (c:d)) = (eqRValue a c) && (eqVLISTA (ValueList b) (ValueList d))
eqVLISTA _ _= False

eqV (V a) (V b)= a==b
eqV _ _= False


--FUNZIONE PRINCIPALE   *)

l = [CLO [Rtn] []]
lazy = lazyE l l
lazyEnv = [lazy]

interpreter:: Stack -> Environment -> [Secdexpr] -> Dump -> RValue
interpreter s e ((Ld (b, n)):c) d = let icst = locate (0,0) lazyEnv
                                        x    = locate (b,n) e
                                    in interpreter (x:s) e c d
interpreter s e ((Ldc k):c) d = trace ("Ldc: " ++ (show k)) $ case k of
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
    trace "Cons" $ interpreter ((ValueList (x:l)):s) e c d
interpreter (x:s) e (Atom:c) d = interpreter ((vatom x):s) e c d
interpreter (b:s) e ((Sel c1 c2):c) d = case b of
    (V (BOO True))  -> (interpreter s e c1 ((CONTR c):d))
    (V (BOO False)) -> (interpreter s e c2 ((CONTR c):d))
interpreter s e (Join:_) ((CONTR c):d) = interpreter s e c d
interpreter s e ((Ldf code):c) d = interpreter ((CLO code e):s) e c d
interpreter (x:_) _ (Rtn:_) ((TRIPLA s e c):d) = interpreter (x:s) e c d
interpreter s e (Push:c) d = interpreter s ([OGA]:e) c d
interpreter (x:s) _ (Stop:_) _ = x
interpreter ((CLO fCode fEnv):(ValueList l):s) e (Ap:c) d =
    trace "Ap" $ interpreter [] (l:fEnv) fCode ((TRIPLA s e c):d)
interpreter ((CLO fCode ([OGA]:fEnv)):(ValueList l):s) e (Rap:c) d =
    trace "Rap" $ interpreter [] ((lazyE l l):fEnv) fCode ((TRIPLA s (tail e) c):d)
interpreter _ _ _ _ = error "Incoherent state"

-- per facilitare l'uso di interpreter
-- se x e'  programma Secdexpr da eseguire. Si aggiunge Stop alla fine.
fin :: [Secdexpr] -> RValue
fin x = interpreter [] [] (x ++ [Stop]) []

run :: String -> RValue
run text = let lexed = lexi text
               (Return (_, lkced)) = prog lexed
               compiled = compile lkced
           in fin compiled

-- Mostra che si puo' usare letrec anche con binders non-funzionali.
-- Le var a sinistra non devono apparire a destra

e = "let z=2 in letrec x= 2+z and y= 2*z in x*y*z end end $"
ee = let z=2 in let x = 2+z
                    y = 2*z
                in x*y*z

-- distribuisce FACT su una lista di interi *)

sss = "letrec FACT = lambda (X) if eq(X, 0) then 1 else X * FACT(X-1) \
     \ and G = lambda(H L) if eq(nil, L) then L else cons(H(car(L)), G(H, cdr(L))) \
     \ in G(FACT, cons(6, cons(7, cons(8, nil)))) end $"

-- (*considera liste di liste Z e produce una lista semplice che contiene tanti
-- interi quante sono le liste contenute in Z e l'intero
-- corrispondente ad una lista contenuta in Z  la somma dei fattoriali dei suoi
-- elementi: f2=fattoriale, f1=calcola somma dei fattori--ali degli elementi di
-- una lista di interi e f0 distribuisce f1 sulle liste contenute in Z *)

f="letrec f0 = lambda ( x ) letrec f1 = lambda(y) letrec f2=lambda (z) if eq(z , 1) then 1 else z * f2( z - 1 ) in if eq( y , nil ) then 0 else f2 ( car ( y ) ) + f1 ( cdr (y)) end in if eq(x , nil) then nil else cons (f1 ( car ( x )),f0 ( cdr ( x ) ) ) end in f0( cons (cons (3 , cons (3 , nil)), cons( cons (3 , nil), nil))) end $"


--(* esempio di funzione che restituisce una funzione locale *)

g="let f1 = lambda() letrec f2 = lambda (z) if eq(z, 1) then 1 else z*f2(z-1) \
                          \ in f2 end \
 \ in let x = f1() in x(8) end end $"
