module Main where

import Lib
import System.IO
import System.Process
import Text.Regex.PCRE

main = do
    let cmd = "wn"
        args = ["dog","-treen"]
        input = ""
    (rc, out, err) <- readProcessWithExitCode cmd args input

    let (_,_,group1,_) = out =~ "Sense 1(.*?)Sense" :: (String,String,String,[String])
    putStrLn group1