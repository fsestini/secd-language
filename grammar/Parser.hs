module Parser (
    Production(..),
    NonTerminal,
    Terminal,
    Symbol,
    parseGrammar
)
where

import Text.ParserCombinators.Parsec

type NonTerminal = String
type Terminal = String
type Symbol = Either NonTerminal Terminal
data Production = Prod NonTerminal [Symbol] deriving (Eq,Ord)
type PairProd = (NonTerminal, [Symbol])

instance Show Production where
    show (Prod nt symbols) =
        nt ++ " -> " ++ (showSymbols symbols)
            where showSymbols [] = "eps"
                  showSymbols [Left s] = s
                  showSymbols [Right s] = s
                  showSymbols (Left s : xs) = s ++ " " ++ showSymbols xs
                  showSymbols (Right s : xs) = s ++ " " ++ showSymbols xs

grammarFile = endBy line eol
line = do a <- ident
          string " -> "
          b <- rightHand
          if b == ["eps"] then return (a, []) else return (a, b)
rightHand = sepBy ident (char ' ')
ident = many (noneOf " ->,\n")
eol = char '\n'

upperCase :: String -> Bool
upperCase (x:_) = x `elem` ['A'..'Z']

lowerCase :: String -> Bool
lowerCase (x:_) = x `elem` ['a'..'z']

lifter :: String -> Symbol
lifter str = if upperCase str then Left str else Right str

prodLifter :: (String, [String]) -> PairProd
prodLifter (nt, l) = (nt, (map lifter l))

parseGr :: String -> [PairProd]
parseGr str = case parse grammarFile "(ijlj)" str of
                    Right list -> map prodLifter list
                    Left err -> error (show err)

parseGrammar :: String -> [Production]
parseGrammar str = let pairs = parseGr str
                       lift = uncurry Prod
                   in map lift pairs
