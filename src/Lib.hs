module Lib
    ( 
        todoFileAvailable,
        printFile,
        createFileIfNotExists
    ) where

import System.Directory
import System.IO
import Data.Char

-- filename of the todo file
fileName :: String
fileName = ".todo"

-- filepath of the todo file
todoFile :: IO FilePath
todoFile = do
    homeDir <- getHomeDirectory
    pure (homeDir ++ "/" ++ fileName)

-- If no todo file exists it will ask the user to create one
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

-- creates the todo file with a sample entry
createFile :: IO ()
createFile = do
    file <- todoFile
    writeFile file "sample entry"
    putStrLn "File created"

-- prints the content of the todo file
printFile :: IO ()
printFile = do
    file <- todoFile
    contents <- readFile file
    putStr contents
    putStr "\n"

-- checks if the todo file is available
todoFileAvailable :: IO Bool
todoFileAvailable = do
    file <- todoFile
    exists <- doesFileExist file
    pure exists
