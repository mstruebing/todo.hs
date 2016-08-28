module Lib
    ( someFunc
    ) where

import System.Directory
import System.IO
import Data.Char

todo_file :: String
todo_file = ".todo"

someFunc :: IO ()
someFunc = do
    fileAvailable <- todoFileAvailable
    if fileAvailable then printFile else createFileIfNotExists

createFileIfNotExists :: IO ()
createFileIfNotExists = do
    putStrLn "No todo-file available (~/.todo)"
    putStrLn "Would you like to create an empty file? [Y/N]"
    input <- getChar
    if ((toUpper input) == 'Y') 
    then do 
        putStrLn "Will create file" 
        createFile
    else 
        return ()
    return ()

createFile :: IO ()
createFile = do
    homeDirectory <- getHomeDirectory
    writeFile (homeDirectory ++ "/" ++ todo_file) "sample entry"
    return ()

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
