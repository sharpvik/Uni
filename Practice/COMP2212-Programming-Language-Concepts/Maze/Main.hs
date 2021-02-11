import System.Environment

import MazeTokens

main :: IO () 
main = getArgs 
    >>= (readFile . head) 
    >>= (print . alexScanTokens)
