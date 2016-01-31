import Lexer
import SyntaxAnalyzer
import Compiler
import Interpreter
import System.Environment

main = do fileNames <- getArgs
          source <- readFile (head fileNames)
          let result = run source
          putStr (show result)
