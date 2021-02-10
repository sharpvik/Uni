import Debug.Trace
import Data.Maybe
import System.Environment



main :: IO ()
main = getArgs >>= multiZipF . head >>= print

-- Use this variant of `main` to tests functions written below.
-- main = mapM_ print tests 



-- Testing Utilities --
data Result = Pass String | Fail String deriving (Show)

assert :: Bool -> String -> Result
assert predicate name = if predicate then Pass name else Fail name



-- Tests --
input1 :: ([Int], [Int])
input1 = ([1..4], [5..8])

input2 :: ([Int], [Int])
input2 = ([1..2], [3..6])

input3 :: [[Int]]
input3 = [[1,2,3],[4,5,6],[7,8,9]]

input4 :: [[Int]]
input4 = [[1,2,3],[4,5,6],[7,8,9],[10],[11],[12,13,14,15]]

tests :: [Result]
tests = 
    [ assert (zipL input1 == [[1,5],[2,6],[3,7],[4,8]]) "zipL"
    , assert (input1 == unzipL (zipL input1)) "unzipL"
    , assert (uncurry zipL' input2 == [[1,3],[2,4],[5],[6]]) "zipL'"
    , assert (multiZipL input3 == [[1,4,7],[2,5,8],[3,6,9]]) "multiZipL"
    , assert (multiZipL input4 == [[1,4,7,10,11,12],[2,5,8,13],[3,6,9,14],[15]])
             "multiZipL (larger input)"
    , assert (parseCSV "1,2,3\n4,5,6" == [[1,2,3],[4,5,6]]) "parseCSV"
    ]



-- Challenges --

-- #1
zipL :: ([a], [a]) -> [[a]]
zipL ([], []) = []
zipL (x:xs, y:ys) = [x, y] : zipL (xs, ys)

unzipL :: [[a]] -> ([a], [a])
unzipL [] = ([], [])
unzipL ([x, y]:rest) = (x:xx, y:yy)
    where (xx, yy) = unzipL rest


-- #2
zipL' :: [a] -> [a] -> [[a]]
zipL' [] [] = []
zipL' [] ys = zipL' ys []
zipL' (x:xs) [] = [x] : zipL' xs []
zipL' (x:xs) (y:ys) = [x, y] : zipL' xs ys

-- It is impossible to write the `unzipL'` function in this case since for lists
-- of type [a] we don't know wheter a came from the left list or the right list.


-- #3
multiZipL :: [[Int]] -> [[Int]]
multiZipL [] = []
multiZipL ls = full zipped
    where
        full = filter (not . null)
        heads = mapMaybe listToMaybe
        tails = map tail . full
        zipped = heads ls : multiZipL (tails ls)


-- #4
multiZipF :: FilePath -> IO [[Int]]
multiZipF = fmap (multiZipL . parseCSV) . readFile

parseCSV :: String -> [[Int]]
parseCSV = map (map read . words . commasAreSpaces) . words
    where commasAreSpaces = map (\c -> if c == ',' then ' ' else c)
