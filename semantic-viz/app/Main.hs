import Lib
import System.IO
import System.Process
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M

-- Builds an adjacency list from a hashmap of key value pairs (word, num_spaces)
--buildAdjacencyList :: [String] -> [Int] -> (Map String Int) -> IO ()
--buildAdjacencyList [] [] hashmap = hashmap
buildAdjacencyList [] prevParent currParent currNumSpaces hashmap = hashmap
buildAdjacencyList pairs prevParent currParent currNumSpaces hashmap = do
    let next = head pairs
        word = head next
        spaces = tail next
    if spaces == currNumSpaces then
        let vals = M.lookup currParent hashmap
            vals = (word:vals)
        M.update currParent vals hashmap
    else if spaces > currNumSpaces then
        let currNumSpaces = spaces
            prevParent = currParent
            currParent = word
        M.insert word [] hashmap
    else if spaces < currNumSpaces then
        let currNumSpaces = spaces
            currParent = prevParent
            vals = M.lookup currParent hashmap
            vals = (word:vals)
        M.update currParent vals hashmap
    buildAdjacencyList pairs prevParent currParent currNumSpaces hashmap


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
    category <- getLine
    let cmd = "app/wc-bash.sh"
        args = [category]
        input = ""
    (rc, out, err) <- readProcessWithExitCode cmd args input

    let inputLines = getLines "app/wn_output.txt"
    nonIOLines <- inputLines
    let pairs = getPairs nonIOLines
        pairs = ((category, 0): pairs)
        emptyMap = M.empty
--        hashmapFromPairs = M.fromList (head pairs) -- construct hashmap of (word, num_spaces) pairs
--        hashmapFromPairs =  M.insert n 0 hashmapFromPairs -- insert root node
        adjacencyList = buildAdjacencyList pairs category category 0 emptyMap
    print $ M.toList hashmap