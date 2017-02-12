module Lib
    ( 
        addTodo,
        createTodoFile,
        printFile,
        removeTodo,
        todoFileAvailable,
    ) where

import System.Directory
import System.IO
import Data.Char

type Todo = String
type TodoNumber = Int

-- filename of the todo file
fileName :: String
fileName = ".todo"

-- filepath of the todo file
todoFile :: IO FilePath
todoFile = do
    homeDir <- getHomeDirectory
    pure (homeDir ++ "/" ++ fileName)

-- creates the todo file with a sample entry
createTodoFile :: IO ()
createTodoFile = do
    file <- todoFile
    writeFile file ""

-- adds a todo
addTodo :: Todo -> IO ()
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
        printTodos $ generateNumberedList contents)

-- print the todolist
-- #. TODOENTRY
printTodos :: [(TodoNumber, Todo)] -> IO ()
printTodos xs = mapM_ (\(a, b) -> putStrLn $ show a ++ ". " ++ b) xs

-- generates a numbered list to identify todos
generateNumberedList :: String -> [(TodoNumber, Todo)]
generateNumberedList = zip [1 ..] . lines 

-- deletes a todo from a list
removeTodo :: TodoNumber -> IO ()
removeTodo x = do
    file <- todoFile
    contents <- fmap lines $ readFile file
    length contents `seq` (writeFile file $ process x contents)
        where
            process x contents = unlines $ concat [take (x - 1) contents, drop x contents]
    

-- checks if the todo file is available
todoFileAvailable :: IO Bool
todoFileAvailable = doesFileExist =<< todoFile

-- is the same as
--todoFileAvailable = do
--    file <- todoFile
--    exists <- doesFileExist file
--    pure exists
-- and
--todoFileAvailable = do
--    file <- todoFile
--    doesFileExist file

