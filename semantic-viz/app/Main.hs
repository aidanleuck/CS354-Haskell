--import Lib
import System.IO
import System.Process
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M


-- add neighbor to current node without updating current parent node pointer
addNeighborSameParent :: [(String, Int)] -> [String] -> Int -> String -> Int -> Map String [String] -> Map String [String]
addNeighborSameParent pairs parents currNumSpaces word spaces hashmap = do
    let currParent = head parents
        vals = M.lookup currParent hashmap  -- Checks if value is in hashmap returns 0 "Hi" for testing purposes
        newVals = (word: (fromMaybe [] vals))
        newParents = (currParent: parents)
        updatedHashmap = M.insert currParent newVals hashmap
    buildAdjacencyList pairs newParents currNumSpaces updatedHashmap


-- update current node to new word and add neighbor
addNeighborNewParent :: [(String, Int)] -> [String] -> Int -> String -> Int -> Map String [String] -> Map String [String]
addNeighborNewParent pairs parents currNumSpaces word spaces hashmap = do
    let newParents = (word:parents)
        currParent = word
        currNumSpaces = spaces
        prevParent = currParent
        updatedHashmap = M.insert word [] hashmap
    buildAdjacencyList pairs newParents currNumSpaces updatedHashmap


-- update current node to old parent and add neighbor
addNeighborOldParent :: [(String, Int)] -> [String] -> Int -> String -> Int -> Map String [String] -> Map String [String]
addNeighborOldParent pairs parents currNumSpaces word spaces hashmap = do
    let currNumSpaces = spaces
        newParents = tail parents
        currParent = last newParents
        vals = M.lookup currParent hashmap
        newVals = (word: (fromMaybe [] vals))
        updatedHashmap = M.insert currParent newVals hashmap
    buildAdjacencyList pairs newParents currNumSpaces updatedHashmap


-- main function for building the adjacency list
buildAdjacencyList :: [(String, Int)] -> [String] -> Int -> Map String [String] -> Map String [String]
buildAdjacencyList [] parents currNumSpaces hashmap = hashmap
buildAdjacencyList pairs parents currNumSpaces hashmap = do
    let currParent = head parents
        (word, spaces) = getFirstTuple pairs
    if spaces > currNumSpaces then addNeighborNewParent (tail pairs) parents currNumSpaces word spaces hashmap
    else if spaces < currNumSpaces then addNeighborOldParent (tail pairs) parents currNumSpaces word spaces hashmap
    else addNeighborSameParent (tail pairs) parents currNumSpaces word spaces hashmap


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
    return pairs -- NOTE: this should not be head, but it won't compile otherwise at the moment


-- Get first tuple from list of tuples
getFirstTuple :: [(String,Int)] -> (String,Int)
getFirstTuple (tuple: tuples) = tuple


getFirstList :: [[(String,Int)]] -> [(String,Int)]
getFirstList (list: lists) = list


main = do
    category <- getLine
--    let cmd = "app/wc-bash.sh"
--        args = [category]
--        input = ""
--    (rc, out, err) <- readProcessWithExitCode cmd args input

    let inputLines = getLines "app/wn_output.txt"
    nonIOLines <- inputLines
    let zippedPairs = getPairs nonIOLines
        pairs = getFirstList zippedPairs
        pairsWithRoot = ((category, 0): pairs)
        emptyMap = M.empty
        hashmapWithRoot = M.insert category [] emptyMap
        adjacencyList = buildAdjacencyList pairsWithRoot [category] 0 hashmapWithRoot
        adjacencyListString = show adjacencyList
    writeFile "app/adjacency_list.txt" adjacencyListString