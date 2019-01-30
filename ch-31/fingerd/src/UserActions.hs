{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Database.SQLite.Simple
import Database.SQLite.Simple.Types
import Data.Text (Text, unpack)
import qualified Data.Text as Text
import System.Environment (getArgs)
import UserData (UserRow, insertQuery, updateQuery)

getUsername :: UserRow -> Text
getUsername (Null, username, _, _, _, _) = username

buildUser :: Text -> Maybe UserRow
buildUser str =
  case Text.splitOn "," str of
    [username, shell, homeDir, realName, phone] ->
      Just (Null, username, shell, homeDir, realName, phone)
    _ -> Nothing

-- Chapter Exercise 2
-- Write an executable separate of fingerd and debug which allows you to add
-- users to the database
addUser :: Text -> IO ()
addUser input = do
  conn <- open "finger.db"
  case buildUser input of
    Nothing -> putStrLn "Invalid Input: Should be five comma separated values"
    Just user -> do
      execute conn insertQuery user
      putStrLn $ "Added user: " <> (unpack $ getUsername user)
  close conn

-- Chapter Exercise 3
-- Add the ability to modify an existing user in the database
updateUser :: Text -> IO ()
updateUser input = do
  conn <- open "finger.db"
  case buildUser input of
    Nothing -> putStrLn "Invalid Input: Should be five comma separated values"
    Just (Null, username, shell, homeDir, realName, phone) -> do
      execute conn updateQuery (shell, homeDir, realName, phone, username)
      putStrLn $ "Updated user: " <> (unpack username)
  close conn

data Action = Add | Update deriving (Eq, Show, Read)

-- Run this from command line:
-- $(stack exec which userActions) "Add" "example1,/bin/bash,/home/example1,Example 1,1919191919"
-- $(stack exec which userActions) "Update" "example1,/bin/sh,/home/examplesh,Example sh,2919191919"
main :: IO ()
main = do
  [action, userData] <- getArgs
  case (read action :: Action) of
    Add -> addUser $ Text.pack userData
    Update -> updateUser $ Text.pack userData
