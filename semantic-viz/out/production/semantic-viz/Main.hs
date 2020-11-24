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

    case out of
        Just out -> parseWN ()
        Nothing -> return ()

--    handle <- openFile "/Users/dvm/Downloads/haskelltxt.txt" ReadMode
--    contents <- System.IO.hGetContents handle
parseWN :: ByteString -> [Char]
parseWN contents = groups where
    r = compile (C.pack "Sense 1(.*?)Sense") [dotall]
    groups = match r (C.pack contents) []
      Prelude.head groups