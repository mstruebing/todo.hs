module Main where

import Options.Applicative
import Lib
import Data.Char
import Data.Monoid((<>))

data TodoApp = TodoApp { 
      todo :: Maybe String 
    , delete :: Maybe Int
    , deleteAll :: Bool }

todoApp :: Parser TodoApp
todoApp = TodoApp
    <$> (optional $ strOption 
        (long "add"
        <> short 'a'
        <> metavar "<TODO>"
        <> help "add todo"))
    <*> (optional $ option auto
        (long "remove"
        <> short 'r'
        <> metavar "<TODONUMBER>"
        <> help "remove todo"))
    <*> switch 
        (long "delete-all"
        <> short 'D'
        <> help "delete all todos")

run :: TodoApp -> IO ()
run (TodoApp todo line False) = executeFunction addOrShowTodos (todo, line)
run (TodoApp _ _ True) = deleteTodos

main :: IO ()
main = execParser opts >>= run
    where 
        opts = info (helper <*> todoApp)
            (fullDesc
            <> progDesc "Manage your todos"
            <> header "A todolist application writte in Haskell")

addOrShowTodos :: (Maybe String, Maybe Int) -> IO ()
addOrShowTodos (Just todo, Nothing) = addTodo todo
addOrShowTodos (Nothing, Just line) = removeTodo line
addOrShowTodos (Just todo, Just line) = do
    addTodo todo
    putStrLn $ show line
addOrShowTodos (_, _) = printFile

deleteTodos :: IO ()
deleteTodos = do
    putStrLn "Are you sure to delete all your todos? [y/N]"
    input <- getChar
    if ((toUpper input) == 'Y')
    then
        createTodoFile
    else
        return ()

-- executes a function if a todo file is available
executeFunction :: (a -> IO ()) -> a -> IO ()
executeFunction f x = do
    fileAvailable <- todoFileAvailable
    if fileAvailable then f x else askToCreateFile

-- asks the user if a todo file should be created
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

