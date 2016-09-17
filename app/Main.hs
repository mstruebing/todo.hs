module Main where

import Options.Applicative
import Lib
import Data.Char

data TodoApp = TodoApp { 
      todo :: Maybe String 
    , show :: Bool
    , delete :: Bool }

todoApp :: Parser TodoApp
todoApp = TodoApp
    <$> (optional $ strOption 
        (long "add"
        <> short 'a'
        <> metavar "<TODO>"
        <> help "add todo"))
    <*> switch 
        (long "show"
        <> short 's'
        <> help "show todos")
    <*> switch 
        (long "delete-all"
        <> short 'D'
        <> help "delete all todos")

run :: TodoApp -> IO ()
run (TodoApp x False False) = addOrShowTodos x
run (TodoApp _ True _) = printFile -- check if file available
run (TodoApp _ False True) = createTodoFile -- ask if file should be deleted

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

