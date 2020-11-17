module Main where

import Lib
import System.IO
import System.Process
import Text.Regex.PCRE.Light
import Data.ByteString.Char8 as C
import Data.Maybe

main = do
    let cmd = "wn"
        args = ["dog", "-treen"]
        input = ""
    (rc, out, err) <- readProcessWithExitCode cmd args input

--    handle <- openFile "/Users/dvm/Downloads/haskelltxt.txt" ReadMode
--    contents <- System.IO.hGetContents handle

    let r = compile (C.pack "Sense 1(.*?)Sense") [dotall]
    let groups = match r (C.pack contents) []
    case groups of
        Just group1 -> C.putStrLn (Prelude.head groups)
        Nothing -> return ()
