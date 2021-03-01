import System.Environment

import qualified Repl
import qualified Eval

main :: IO () 
main = getArgs >>= (mode . head) 

mode :: String -> IO ()
mode "repl" = Repl.loop
mode file = readFile file >>= Eval.evalAndPrint
