module Lib
    ( someFunc
    ) where

import System.Directory
import System.IO

todo_file :: String
todo_file = ".todo"

someFunc :: IO ()
someFunc = do
    fileAvailable <- todoFileAvailable
    if fileAvailable then printFile else putStrLn "No file available"

printFile :: IO ()
printFile = do
    homeDirectory <- getHomeDirectory
    contents <- readFile (homeDirectory ++ "/" ++ todo_file)
    putStr contents

todoFileAvailable :: IO Bool
todoFileAvailable = do
    homeDirectory <- getHomeDirectory
    exists <- doesFileExist (homeDirectory ++ "/" ++ todo_file)
    pure exists
