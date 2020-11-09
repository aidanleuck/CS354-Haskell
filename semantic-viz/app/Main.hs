module Main where

import Lib
import System.Process

main = do
    let cmd = "ls"
        args = ["-l"]
        input = ""
    (rc, out, err) <- readProcessWithExitCode cmd args input
    putStrLn $ "test: " ++ show rc
    mapM_ putStrLn $ map ("out: " ++) $ lines out
    mapM_ putStrLn $ map ("err: " ++) $ lines err


