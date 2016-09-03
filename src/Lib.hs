module Lib
    ( 
        addTodo,
        createTodoFile,
        printFile,
        todoFileAvailable,
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
createTodoFile :: IO ()
createTodoFile = do
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
    writeFile file "sample entry\n"
    putStrLn "File created"

-- adds a todo
addTodo :: String -> IO ()
addTodo x = do
    file <- todoFile
    appendFile file $ x ++ "\n"

-- prints the content of the todo file
printFile :: IO ()
printFile = do
    file <- todoFile
    withFile file ReadMode (\handle -> do
        hSetBuffering handle $ LineBuffering
        contents <- hGetContents handle
        printTodos $ generateNumberedList contents
        )

-- print the todolist
-- #. TODOENTRY
printTodos :: [(Int, String)] -> IO ()
printTodos xs = mapM_ (\(a, b) -> putStrLn $ show a ++ ". " ++ b) xs

-- generates a numbered list to identify todos
generateNumberedList :: String -> [(Int, String)]
generateNumberedList todos = zip [1 .. numberOfTodos] todoList 
    where
        numberOfTodos = length todoList
        todoList = lines todos

-- checks if the todo file is available
todoFileAvailable :: IO Bool
todoFileAvailable = do
    file <- todoFile
    exists <- doesFileExist file
    pure exists
