module Repl ( Repl.read, Repl.loop ) where


import System.IO as Buf
import Data.Foldable
import Control.Monad

import qualified Eval
import qualified Parser


-- read prints out some helpful prefix to express that it's 
-- expecting some input; then it reads one line of input and
-- returns it.
read :: IO String
read = do
    printFlush "λ "
    getLine


-- loop spins up an infinite loop that reads from stdin, evaluates, and prints
-- the evaluated expression to stdout.
loop = forever (Repl.read >>= Eval.evalAndPrint)


---------- UTILITY FUNCTIONS ---------------------------------------------------

printFlush :: String -> IO ()
printFlush string = do
    Prelude.putStr string
    flush


flush :: IO ()
flush = Buf.hFlush stdout
