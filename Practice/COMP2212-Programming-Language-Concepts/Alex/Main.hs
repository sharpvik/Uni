import System.Environment

import Tokens

main :: IO () 
main = getArgs 
    >>= (readFile . head) 
    >>= (print . alexScanTokens)
