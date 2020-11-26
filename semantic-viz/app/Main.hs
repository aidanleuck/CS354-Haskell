import Lib
import System.IO
import System.Process
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M

-- Builds an adjacency list using a list of words and their corresponding number of leading spaces
--buildAdjacencyList :: [String] -> [Int] -> (Map String Int) -> IO ()
--buildAdjacencyList [] [] hashmap = hashmap
buildAdjacencyList (k:keys) (v:vals) hashmap = do
    let map1 = M.insert k v hashmap
    print $ M.toList map1
    return map1
        --map2 = buildAdjacencyList keys vals map1
    -- return map1


-- Counts spaces
isSpace s = s == ' '
countNumSpaces str = length (filter isSpace str)


-- Splits a line on the '=' character. Used by the getPairs function.
splitOnEqualSign :: [Char] -> [String]
splitOnEqualSign "" = [""]
splitOnEqualSign ('=':cs) = "" : splitOnEqualSign cs
splitOnEqualSign (c:cs) = (c:cellCompletion) : otherCells
 where cellCompletion : otherCells = splitOnEqualSign cs


-- Gets lines from input file containing wordnet output, and removes headers from top
getLines :: String -> IO [String]
getLines fileName = do
    let inputLines = drop 7 . lines <$> readFile fileName
    outLines <- inputLines
    return outLines


-- Inputs lines of Wordnet output and returns pairs of words + number of leading spaces for that input line
getPairs :: [String] -> [[(String, Int)]]
getPairs inputLines = do
    let splitLines = map splitOnEqualSign inputLines  -- split input lines on '=' character
        spaces = map head splitLines -- get leading spaces on each line
        numSpaces = map length spaces -- measure number of leading spaces on each line
        wordStrings = map head (map tail (map words inputLines)) -- get words on each line
        pairs = zip wordStrings numSpaces
    return pairs

main = do
    n <- getLine
    let cmd = "app/wc-bash.sh"
        args = [n]
        input = ""
    (rc, out, err) <- readProcessWithExitCode cmd args input

    let inputLines = getLines "app/wn_output.txt"
    nonIOLines <- inputLines
    let pairs = getPairs nonIOLines
--        hashmap = M.fromList pairs
--        adjacencyList =  M.insert n 0 hashmap -- insert root node
--        adjacencyList = buildAdjacencyList pairs hashmap
--    print $ M.toList hashmap
    print pairs