module Main where

import Lib
import System.Process

main = do
    let cmd = "ls"
        args = ["-l"]
        input = ""
    (rc, out, err) <- readProcessWithExitCode cmd args input
    putStrLn $ "exit code: " ++ show rc
    mapM putStrLn $ map ("out: " ++) $ lines out
    mapM putStrLn $ map ("err: " ++) $ lines err


