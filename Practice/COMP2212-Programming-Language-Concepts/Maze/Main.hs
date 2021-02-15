import System.Environment

import Lexer
import Parser

main :: IO () 
main = getArgs 
    >>= (readFile . head) 
    >>= (print . parse . tokenize)
