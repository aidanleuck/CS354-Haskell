import Lib
import System.IO
import System.Process
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M


-- add neighbor to current node without updating current parent node pointer
addNeighborSameParent currParent currNumSpaces word spaces hashmap = do
    let vals = M.lookup currParent hashmap
        newVals = (word:vals)
    M.update currParent newVals hashmap
    buildAdjacencyList pairs prevParent currParent currNumSpaces hashmap


-- update current node to new word and add neighbor
addNeighborNewParent currParent currNumSpaces word spaces hashmap = do
    let currNumSpaces = spaces
        prevParent = currParent
        currParent = word
    M.insert word [] hashmap
    buildAdjacencyList pairs prevParent currParent currNumSpaces hashmap


-- update current node to old parent and add neighbor
addNeighborOldParent currParent currNumSpaces word spaces hashmap = do
    let currNumSpaces = spaces
        currParent = prevParent
        vals = M.lookup currParent hashmap
        newVals = (word:vals)
    M.update currParent newVals hashmap
    buildAdjacencyList pairs prevParent currParent currNumSpaces hashmap

-- main function for building the adjacency list
buildAdjacencyList [] parents currNumSpaces hashmap = hashmap
buildAdjacencyList pairs parents currNumSpaces hashmap = do
    let next = head pairs
        word = head next
        spaces = last next
        currParent = last parents
    if spaces > currNumSpaces then addNeighborNewParent currParent currNumSpaces word spaces hashmap
    else if spaces < currNumSpaces then addNeighborOldParent currParent currNumSpaces word spaces hashmap
    else addNeighborSameParent currParent currNumSpaces word spaces hashmap


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
        wordStrings = map last (map words inputLines) -- get words on each line
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
        hashmap = M.insert category 0 emptyMap
--        hashmapFromPairs = M.fromList (head pairs) -- construct hashmap of (word, num_spaces) pairs
--        hashmapFromPairs =  M.insert n 0 hashmapFromPairs -- insert root node
        adjacencyList = buildAdjacencyList pairs [category] 0 hashmap
--    print $ M.toList hashmap
    print pairs