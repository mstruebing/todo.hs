module Main where

import Options.Applicative
import Lib
import Data.Char

data TodoApp = TodoApp { 
      todo :: Maybe String 
    , delete :: Bool }

todoApp :: Parser TodoApp
todoApp = TodoApp
    <$> (optional $ strOption 
        (long "add"
        <> short 'a'
        <> metavar "<TODO>"
        <> help "add todo"))
    <*> switch 
        (long "delete-all"
        <> short 'D'
        <> help "delete all todos")

run :: TodoApp -> IO ()
run (TodoApp x False) =  execute addOrShowTodos x
run (TodoApp _ True) = deleteTodos

main :: IO ()
main = execParser opts >>= run
    where 
        opts = info (helper <*> todoApp)
            (fullDesc
            <> progDesc "Manage your todos"
            <> header "A todolist application writte in Haskell")

addOrShowTodos :: Maybe String -> IO ()
addOrShowTodos Nothing = printFile
addOrShowTodos (Just x) = addTodo $ x

deleteTodos :: IO ()
deleteTodos = do
    putStrLn "Are you sure to delete all your todos? [Y/N]"
    input <- getChar
    if ((toUpper input) == 'Y')
    then
        createTodoFile
    else
        return ()

execute :: (a -> IO ()) -> a -> IO ()
execute f x = do
    fileAvailable <- todoFileAvailable
    if fileAvailable then f x else askToCreateFile

askToCreateFile :: IO ()
askToCreateFile = do
    putStrLn "No todo file is available(~/.todo)"
    putStrLn "Would you like to create one? [Y/N]"
    input <- getChar
    if ((toUpper input) == 'Y')
    then
        createTodoFile
    else
        return ()

