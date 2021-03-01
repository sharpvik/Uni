module Eval where

import Lexer
import Parser

eval :: String -> Exp
eval = parse . tokenize

evalAndPrint :: String -> IO ()
evalAndPrint = putStrLn . (++ "\n") . show . eval
