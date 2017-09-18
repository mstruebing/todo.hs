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
    askToOverwriteFile "Are you sure to delete all your todos?"

-- same as
-- input <- getChar
-- if ((toUpper input) == 'Y')
-- then
--     createTodoFile
-- else
--     return ()

-- executes a function if a todo file is available
executeFunction :: (a -> IO ()) -> a -> IO ()
executeFunction f x = todoFileAvailable >>= (\fileAvailable -> if fileAvailable then f x else noFileAvailable)

-- same as
-- fileAvailable <- todoFileAvailable
-- if fileAvailable then f x else askToCreateFile

noFileAvailable :: IO ()
noFileAvailable = do
    putStrLn "No todo file is available(~/.todo)"
    askToOverwriteFile "Would you like to create one?"

askToOverwriteFile :: String -> IO ()
askToOverwriteFile question = do
    putStrLn $ question ++ " [y/N]"
    getChar >>= (\char -> if toUpper char == 'Y' then createTodoFile else return ())

    -- same as
    -- input <- getChar
    -- if ((toUpper input) == 'Y')
    -- then
    --     createTodoFile
    -- else
    --     return ()

