import System.Environment

import qualified Repl

main :: IO () 
main = getArgs >>= mode 

mode :: [String] -> IO ()
mode [] = Repl.loop
mode ["repl"] = Repl.loop
mode [file] = readFile file >>= Repl.cycle
mode _ = putStrLn "Invlid invocation!\nUsage: Toy [repl | <file.toy>]"
