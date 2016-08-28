module Lib
    ( someFunc
    ) where

import System.Directory
import System.IO
import Data.Char

fileName :: String
fileName = ".todo"

todoFile :: IO FilePath
todoFile = do
    homeDir <- getHomeDirectory
    pure (homeDir ++ "/" ++ fileName)

someFunc :: IO ()
someFunc = do
    fileAvailable <- todoFileAvailable
    if fileAvailable then printFile else createFileIfNotExists

createFileIfNotExists :: IO ()
createFileIfNotExists = do
    putStrLn "No todo-file available (~/.todo)"
    putStrLn "Would you like to create an empty file? [Y/N]:"
    input <- getChar
    if ((toUpper input) == 'Y') 
    then 
        createFile
    else 
        return ()

createFile :: IO ()
createFile = do
    file <- todoFile
    writeFile file "sample entry"
    putStrLn "File created"

printFile :: IO ()
printFile = do
    file <- todoFile
    contents <- readFile file
    putStr contents

todoFileAvailable :: IO Bool
todoFileAvailable = do
    file <- todoFile
    exists <- doesFileExist file
    pure exists
