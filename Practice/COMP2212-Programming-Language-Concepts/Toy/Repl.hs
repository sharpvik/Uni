module Repl ( Repl.cycle, Repl.loop ) where


import System.IO as Buf
import Data.Foldable
import Control.Monad

import qualified Eval


-- read prints out some helpful prefix to express that it's 
-- expecting some input; then it reads one line of input and
-- returns it.
read :: IO String
read = do
    printFlush "λ <- "
    getLine


print :: String -> IO ()
print = putStrLn . (++ "\n") . ("λ -> " ++)


cycle :: String -> IO ()
cycle = Repl.print . show . Eval.eval


-- loop spins up an infinite loop that reads from stdin, evaluates, and prints
-- the evaluated expression to stdout.
loop :: IO ()
loop = forever $ Repl.read >>= Repl.cycle


---------- UTILITY FUNCTIONS ---------------------------------------------------

printFlush :: String -> IO ()
printFlush string = do
    Prelude.putStr string
    flush


flush :: IO ()
flush = Buf.hFlush stdout
