import System.Environment

import qualified Tokens
import qualified Grammar

main :: IO () 
main = getArgs 
    >>= (readFile . head) 
    >>= (print . Grammar.parseCalc . Tokens.alexScanTokens)

