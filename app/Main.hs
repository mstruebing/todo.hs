module Main where

import Lib

main :: IO ()
main = do
    fileAvailable <- todoFileAvailable
    if fileAvailable then printFile else createTodoFile

