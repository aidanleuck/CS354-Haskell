import System.IO
import System.Process
import Data.Maybe
import Data.Map (Map)
import Data.List.Unique
import qualified Data.Map as M


-- Recursive(-ish) function for building the adjacency list. Uses number of leading spaces of each line of
-- output from Wordnet to determine node placement in graph.
-- NOTE: I say recursive-ish because the helper functions it calls all call this function again after
--       completing their respective operations (i.e. tail recursion)
buildAdjacencyList :: [(String, Int)] -> [String] -> Int -> Map String [String] -> Map String [String]
buildAdjacencyList [] parents currNumSpaces hashmap = hashmap
buildAdjacencyList pairs parents currNumSpaces hashmap = do
    let (word, spaces) = getFirstTuple pairs
    if spaces > currNumSpaces then addNeighborNewParent (tail pairs) parents currNumSpaces word spaces hashmap
    else if spaces < currNumSpaces then addNeighborOldParent (tail pairs) parents currNumSpaces word spaces hashmap
    else addNeighborSameParent (tail pairs) parents currNumSpaces word spaces hashmap


-- Add neighbor to current node without updating current parent node pointer
addNeighborSameParent :: [(String, Int)] -> [String] -> Int -> String -> Int -> Map String [String] -> Map String [String]
addNeighborSameParent pairs parents currNumSpaces word spaces hashmap = do
    let currParent = head parents
        -- add parent -> node relationship
        vals = M.lookup currParent hashmap
        newVals = uniq (word: (fromMaybe [] vals))
        updatedHashmap = M.insert currParent newVals hashmap

        -- add node -> parent relationship
        vals2 = M.lookup word hashmap
        newVals2 = uniq (currParent: (fromMaybe [] vals2))
        updatedHashmap2 = M.insert word newVals2 updatedHashmap
    buildAdjacencyList pairs parents currNumSpaces updatedHashmap2


-- Update current node to new word and add neighbor
addNeighborNewParent :: [(String, Int)] -> [String] -> Int -> String -> Int -> Map String [String] -> Map String [String]
addNeighborNewParent pairs parents currNumSpaces word spaces hashmap = do
    let prevParent = head parents
        newParents = (word:parents)
        currParent = word
        currNumSpaces = spaces

        -- add node -> parent relationship
        updatedHashmap = M.insert word [prevParent] hashmap

        -- add parent -> node relationship
        updatedHashmap2 = M.insert prevParent [word] hashmap
    buildAdjacencyList pairs newParents currNumSpaces updatedHashmap


-- Update current node to old parent and add neighbor
addNeighborOldParent :: [(String, Int)] -> [String] -> Int -> String -> Int -> Map String [String] -> Map String [String]
addNeighborOldParent pairs parents currNumSpaces word spaces hashmap = do
    let newParents = tail parents
        currParent = head newParents
        currNumSpaces = spaces

        -- add parent -> node relationship
        vals = M.lookup currParent hashmap
        newVals = uniq (word: (fromMaybe [] vals))
        updatedHashmap = M.insert currParent newVals hashmap

        -- add node -> parent relationship
        vals2 = M.lookup word hashmap
        newVals2 = uniq (currParent: (fromMaybe [] vals2))
        updatedHashmap2 = M.insert word newVals2 hashmap
    buildAdjacencyList pairs newParents currNumSpaces updatedHashmap2


-- Counts spaces
isSpace s = s == ' '
countNumSpaces str = length (filter isSpace str)


-- Splits a line on the '=' character. Used by the getPairs function.
splitOnEqualSign :: [Char] -> [String]
splitOnEqualSign "" = [""]
splitOnEqualSign ('=':cs) = "" : splitOnEqualSign cs
splitOnEqualSign (c:cs) = (c:cellCompletion) : otherCells
 where cellCompletion : otherCells = splitOnEqualSign cs

-- Splits a line on the ',' character. Used by the getPairs function.
splitOnCommas :: [Char] -> [String]
splitOnCommas "" = [""]
splitOnCommas (',':cs) = "" : splitOnCommas cs
splitOnCommas (c:cs) = (c:cellCompletion) : otherCells
 where cellCompletion : otherCells = splitOnCommas cs


-- Gets lines from input file containing wordnet output, and removes headers from top
getLines :: String -> IO [String]
getLines fileName = do
    let inputLines = drop 3 . lines <$> readFile fileName
    outLines <- inputLines
    return outLines


-- Inputs lines of Wordnet output and returns pairs of words + number of leading spaces for that input line
getPairs :: [String] -> [[(String, Int)]]
getPairs inputLines = do
    let splitLines = map splitOnEqualSign inputLines  -- split input lines on '=' character
        spaces = map head splitLines -- get leading spaces on each line
        numSpaces = map length spaces -- measure number of leading spaces on each line
        wordStrings = map removeCommas (map getFirstWord (map words inputLines)) -- get words on each line
        pairs = zip wordStrings numSpaces
    return pairs


-- Get first tuple from list of tuples
getFirstTuple :: [(String,Int)] -> (String,Int)
getFirstTuple (tuple: tuples) = tuple


-- Get first item from list of lists
getFirstList :: [[(String,Int)]] -> [(String,Int)]
getFirstList (list: lists) = list


-- Get second element of each list if length is > 1, else return first element
getFirstWord (x:y:xs) = do
    if (null y) then x
    else y


-- Check if item already exists in list
isDuplicate _ [] = False
isDuplicate x (y : ys) = if x == y then True else isDuplicate x ys


-- Remove commas from string
removeCommas xs = [ x | x <- xs, not (x `elem` ",") ]


-- Use matplotlib to visualize graph
visualize filename word1 word2 = do
    let cmd = "python3"
        args = ["app/visualize.py", filename, word1, word2]
        input = ""
    (rc, out, err) <- readProcessWithExitCode cmd args input
    putStrLn "Visualizing semantic graph with Matplotlib..."


-- Main function with the following steps
-- 1. Runs Wordnet with user specified input word
-- 2. Parses Wordnet output into an adjacency list representing an undirected graph
--    of semantic relationships between the related words
-- 3. Display the undirected graph and visualize the shortest distance between the 2 input words
main = do
    -- get user input
    putStrLn "Enter a word/category to visualize the semantic relationships with related words:"
    category <- getLine
    putStrLn "Enter word 1 (source) for calculating semantic distance:"
    word1 <- getLine
    putStrLn "Enter word 1 (target) for calculating semantic distance:"
    word2 <- getLine

    -- run wordnet
    let cmd = "app/wc-bash.sh"
        args = [category]
        input = ""
    (rc, out, err) <- readProcessWithExitCode cmd args input

    -- parse wordnet output into adjacency list
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

    -- display undirected graph and visualize shortest distance between input words
    visualize "app/adjacency_list.txt" word1 word2