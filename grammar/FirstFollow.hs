module FirstFollow where

import Data.Set
import Parser

data FirstOutput = Terminal Terminal | Epsilon deriving (Show,Eq,Ord)

type First = Symbol -> Set FirstOutput
type Follow = NonTerminal -> Set Terminal
type ParseTable = NonTerminal -> Terminal -> Set Production

shuffle :: [a] -> [b] -> [(a,b)]
shuffle l1 l2 = [(x,y) | x <- l1, y <- l2]

image :: (Ord a, Ord b) => (a -> b) -> Set a -> Set (a,b)
image f = Data.Set.map (\x -> (x, f x))

parseTable :: Set Production -> First -> Follow -> NonTerminal -> Terminal
              -> Set Production
parseTable productions first follow nonterm term =
    let myProds = productions `prodFrom` nonterm
    in Data.Set.filter (isParseProd first follow nonterm term) myProds

isParseProd :: First -> Follow -> NonTerminal -> Terminal -> Production -> Bool
isParseProd first follow x a (Prod _ w) =
    member (Terminal a) (firstOfString first w)
    || (member Epsilon (firstOfString first w) && member a (follow x))

isLL1 :: ParseTable -> Set NonTerminal -> Set Terminal -> Bool
isLL1 pTable nonterms terms =
    let all = shuffle (toList nonterms) (toList terms)
        mapall = Prelude.map (\(x,y) -> length (pTable x y) <= 1) all
    in and mapall

offendingPairs :: ParseTable -> Set NonTerminal -> Set Terminal
                  -> [(NonTerminal,Terminal)]
offendingPairs pTable nonterms terms =
    let all = shuffle (toList nonterms) (toList terms)
    in Prelude.filter (\(x,y) -> length (pTable x y) > 1) all

getSymbols :: Set Production -> Set Symbol
getSymbols productions = let f (Prod p l) = fromList (Left p : l)
                         in fold union empty (Data.Set.map f productions)

getNonTerminals :: Set Symbol -> Set NonTerminal
getNonTerminals sss = let f (Left _) = True
                          f _ = False
                          nonTerms = Data.Set.filter f sss
                      in Data.Set.map (\(Left x) -> x) nonTerms

getTerminals :: Set Symbol -> Set NonTerminal
getTerminals sss = let f (Right _) = True
                       f _ = False
                       terms = Data.Set.filter f sss
                   in Data.Set.map (\(Right x) -> x) terms

extractNonterminal :: Production -> NonTerminal
extractNonterminal (Prod x _) = x

prodFrom :: Set Production -> NonTerminal -> Set Production
prodFrom prods nont = Data.Set.filter ((nont ==) . extractNonterminal) prods

limit :: (Ord a, Ord b) => Set a -> [a -> b] -> a -> b
limit domain (s1:s2:sx) = if image s1 domain == image s2 domain
                          then s1 else limit domain (s2:sx)

-----------------------------------------

{- Compute the first of a production -}
firstOfProduction :: First -> Production -> Set FirstOutput
firstOfProduction first (Prod _ str) = firstOfString first str

{- Compute the first of a string -}
firstOfString :: First -> [Symbol] -> Set FirstOutput
firstOfString _ [] = singleton Epsilon
firstOfString first (w:wx) = case w of
    Right terminal   -> singleton (Terminal terminal)
    Left nonTerminal -> let ff = first w
                        in union (difference ff (singleton Epsilon))
                            (if Epsilon `member` ff
                             then firstOfString first wx
                             else empty)

-- Compute First_i from First_{i-1}
firstVector :: Set Production -> First -> First
firstVector productions oldFirst x = case x of
    Right terminal   -> singleton (Terminal terminal)
    Left nonTerminal -> let prods = productions `prodFrom` nonTerminal
                            asd = Data.Set.map (firstOfProduction oldFirst) prods
                        in fold union empty asd

-----------------------------------------

{- Return the part of a production that is at the right of a specified
 - nonterminal -}
rightOf :: NonTerminal -> [Symbol] -> [Symbol]
rightOf _ [] = error "rightOf failed"
rightOf x (Left y : l) = if x == y then l else rightOf x l
rightOf x (Right _ : l) = rightOf x l

{- Return productions that have the specified nonterminal in
 - the right-hand side -}
prodWith :: Set Production -> NonTerminal -> Set Production
prodWith productions nonTerm =
    Data.Set.filter (\(Prod x y) -> (Left nonTerm) `elem` y) productions

{- Compute follow for a production -}
followProd :: First -> Follow -> NonTerminal -> Production -> Set Terminal
followProd first follow nont (Prod y axb) =
    let b = rightOf nont axb in
        if Prelude.null b then follow y else
            let ff = firstOfString first b
                dif = difference ff (singleton Epsilon)
                diff = Data.Set.map (\(Terminal t) -> t) dif
            in if member Epsilon ff then diff `union` follow y else diff

{- Compute Follow_i from Follow_{i-1} -}
followVector :: Set Production -> First -> Follow -> Follow
followVector productions first oldFollow nont =
    let prods = prodWith productions nont
        res = fold union empty (Data.Set.map (followProd first oldFollow nont) prods)
    in res `union` oldFollow nont
