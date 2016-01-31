import Parser
import FirstFollow
import System.Environment
import Data.Set

prettify :: (Show a, Show b) => (a,b) -> String
prettify (a,b) = (show a) ++ " -> " ++ (show b)

getFollow :: NonTerminal -> Set Production -> Set NonTerminal -> First -> Follow
getFollow startSym productions nonTerminals first =
    let follow0 s = if s == startSym then singleton "$" else empty
        followSequence = follow0:(Prelude.map (followVector productions first) followSequence)
    -- 'Follow' as the result of a fixed point computation
    in limit nonTerminals followSequence

getFirst :: Set Production -> Set Symbol -> First
getFirst productions symbols =
    let first0 = const empty
        firstSequence = first0 : (Prelude.map (firstVector productions) firstSequence)
    -- 'First' as the result of a fixed point computation
    in limit symbols firstSequence

startSymbol :: [Production] -> NonTerminal
startSymbol [] = error "empty grammar"
startSymbol prods = (extractNonterminal . head) prods

main = do args <- getArgs
          let filePath = head args
          file <- readFile filePath
          let productionList = parseGrammar file
              productions = fromList productionList
              startSym = startSymbol productionList
              symbols = getSymbols productions
              nonTerminals = getNonTerminals symbols
              terminals = getTerminals symbols
              first = getFirst productions symbols
              follow = getFollow startSym productions nonTerminals first
              table = parseTable productions first follow
              ll1 = isLL1 table nonTerminals terminals
          print (if ll1 then "Grammar is LL1"
                  else "Grammar is not LL1\n"
                       ++ (let chain [] = ""
                               chain (s:ss) = s ++ "\n" ++ chain ss
                               offending = offendingPairs table nonTerminals terminals
                           in chain (fmap prettify offending)))

printProductions :: [Production] -> IO ()
printProductions prods = sequence_ ((pure print) <*> ((pure show) <*> prods))
