module Main where

import System.Environment (getArgs)
import Database.SQLite.Simple
import UserServer (handleUpdate)

-- Run this from command line:
-- $(stack exec which userActionsExe) "Add" "example1,/bin/bash,/home/example1,Example 1,1919191919"
-- $(stack exec which userActionsExe) "Update" "example1,/bin/sh,/home/examplesh,Example sh,2919191919"
main :: IO ()
main = do
  conn <- open "finger.db"
  [action, userData] <- getArgs
  handleUpdate conn action userData
  close conn
