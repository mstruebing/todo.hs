module Lib
    ( someFunc
    ) where

import System.Directory

someFunc :: IO ()
someFunc = homeDirectoryAvailable >>= print
--    exists <- homeDirectoryAvailable
--    print exists

homeDirectoryAvailable :: IO Bool
homeDirectoryAvailable  = do
    homeDirectory <- getHomeDirectory
    exists <- doesDirectoryExist homeDirectory
    pure exists
