import Lib
import System.IO
import System.Process
import Text.Regex.PCRE.Light
--import Data.ByteString.Char8 as C
import Data.Maybe


-- function for counting spaces on a line, which we use to determine node placement in graph
-- countNumSpaces str = length ([x | x <- str, x == ' '])
-- countNumSpaces str = length (filter (\x -> x == ' ') str)
isSpace s = s == ' '
countNumSpaces str = length (filter isSpace str)

data Graph a = Graph [(a,[a])] -- Graph is a list of origins paired with edgeends

createGraph ::Eq a => [(a,a)] -> Graph a
createGraph = undefined

empty :: Graph a
empty = Graph []

insertVertex :: Eq a => a -> Graph a -> Graph a
insertVertex = undefined -- insert if not already in the Graph (with empty edges)

insertEdge :: Eq a => (a,a) -> Graph a -> Graph a
insertEdge = undefined -- insert edge in list of origin
--do not forget to add origin, end if they don't exist

split :: [Char] -> [String]
split "" = [""]
split ('=':cs) = "" : split cs
split (c:cs) = (c:cellCompletion) : otherCells
 where cellCompletion : otherCells = split cs


parseFile :: String -> IO String
parseFile fileName = do
    contents <- readFile fileName
    let inputLines = lines contents
    return (unlines inputLines)

main = do
--    let cmd = "wn"
--        args = ["dog", "-treen"]
--        input = ""
--    (rc, out, err) <- readProcessWithExitCode cmd args input

    let x = parseFile "haskelltxt.txt"
    y <- x
    putStrLn y



--    case contents of
--        Just contents -> parse contents
--        Nothing -> return ()

--parse :: [Char] -> IO ()
--parse contents = group1 where
--    r = compile (C.pack "Sense 1(.*?)Sense") [dotall]
--    groups = match r (C.pack contents) []
--    group1 = Prelude.head groups
--    System.IO.putStrLn group1