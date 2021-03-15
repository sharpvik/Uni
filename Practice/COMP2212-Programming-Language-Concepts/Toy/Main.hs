import System.Environment

import qualified Repl

main :: IO () 
main = getArgs >>= mode 

mode :: [String] -> IO ()
mode [] = help
mode ["help"] = help
mode ["repl"] = Repl.loop
mode [file] = readFile file >>= Repl.cycle
mode _ = putStrLn $ "Invlid invocation!\n" ++ usage

help :: IO ()
help = putStrLn usage

usage :: String
usage = "Usage: Toy [help | repl | <file.toy>]"
