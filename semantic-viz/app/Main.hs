import Lib
import System.IO
import System.Process
import Data.Maybe
import qualified Data.Map as M

--buildAdjacencyList :: [Int] -> [String] -> M
buildAdjacencyList keys vals hashmap = do
    let map1 = M.insert 5 "Four" hashmap
        map2 = M.insert 5 "Five" map1
    return map2


isSpace s = s == ' '
countNumSpaces str = length (filter isSpace str)

split :: [Char] -> [String]
split "" = [""]
split ('=':cs) = "" : split cs
split (c:cs) = (c:cellCompletion) : otherCells
 where cellCompletion : otherCells = split cs

getLines :: String -> IO [String]
getLines fileName = do
    let inputLines = drop 4 . lines <$> readFile fileName
    outLines <- inputLines
    return outLines

getSpaces :: [String] -> [Int]
getSpaces inputLines = do
    let splitLines = map split inputLines
    let spaces = map head splitLines
    let num_spaces = map length spaces
    return (head num_spaces)

main = do
    n <- getLine
    let cmd = "app/wc-bash.sh"
        args = [n]
        input = ""
    (rc, out, err) <- readProcessWithExitCode cmd args input

    let inputLines = getLines "app/wn_output.txt"
    nonIOLines <- inputLines
    let inputSpaces = getSpaces nonIOLines
        emptyHashMap = M.empty
        adjacencyList = buildAdjacencyList inputSpaces inputLines
    -- mapM_ (print . snd) (M.toList adjacencyList)
    print $ (M.toList adjacencyList)