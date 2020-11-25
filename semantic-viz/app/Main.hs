import Lib
import System.IO
import System.Process
import Text.Regex.PCRE.Light
--import Data.ByteString.Char8 as C
import Data.Maybe

isSpace s = s == ' '
countNumSpaces str = length (filter isSpace str)

split :: [Char] -> [String]
split "" = [""]
split ('=':cs) = "" : split cs
split (c:cs) = (c:cellCompletion) : otherCells
 where cellCompletion : otherCells = split cs


parseFile :: String -> IO String
parseFile fileName = do
    let inputLines = drop 4 . lines <$> readFile fileName
    outLines <- inputLines
    let splitLines = map split outLines
    let spaces = map head splitLines
    let num_spaces = map length spaces
    print num_spaces
    return (unlines outLines)

main = do
--    let cmd = "wn"
--        args = ["dog", "-treen"]
--        input = ""
--    (rc, out, err) <- readProcessWithExitCode cmd args input

    let x = parseFile "app/wn_output.txt"
    y <- x
    putStrLn y