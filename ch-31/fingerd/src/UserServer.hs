{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module UserServer where

import Database.SQLite.Simple
import Database.SQLite.Simple.Types
import Data.Text (Text, unpack)
import qualified Data.Text as Text
import UserData (UserRow, insertQuery, updateQuery)
import Network.Socket.ByteString (recv)
import Network.Socket hiding (close, recv)
import Data.Text.Encoding (decodeUtf8)
import Control.Monad (forever)

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
addUser :: Connection -> Text -> IO ()
addUser conn input =
  case buildUser input of
    Nothing -> putStrLn "Invalid Input: Should be five comma separated values"
    Just user -> do
      execute conn insertQuery user
      putStrLn $ "Added user: " <> (unpack $ getUsername user)

-- Chapter Exercise 3
-- Add the ability to modify an existing user in the database
updateUser :: Connection -> Text -> IO ()
updateUser conn input =
  case buildUser input of
    Nothing -> putStrLn "Invalid Input: Should be five comma separated values"
    Just (Null, username, shell, homeDir, realName, phone) -> do
      execute conn updateQuery (shell, homeDir, realName, phone, username)
      putStrLn $ "Updated user: " <> (unpack username)

data Action = Add | Update deriving (Eq, Show, Read)

handleUpdate :: Connection -> String -> String -> IO ()
handleUpdate conn action userData =
  case (read action :: Action) of
    Add -> do
      putStrLn "Adding new user..."
      addUser conn $ Text.pack userData
    Update -> do
      putStrLn "Updating user..."
      updateUser conn $ Text.pack userData

handleUpdates :: Connection -> Socket -> IO ()
handleUpdates conn sock = forever $ do
  (soc, _) <- accept sock
  putStrLn "Got connection, handling query"
  msg <- recv soc 1024
  let [action, userData] = words (unpack $ decodeUtf8 msg)
  handleUpdate conn action userData
  close' soc

userServer :: String -> IO ()
userServer port = do
  addrinfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                           Nothing (Just port)
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr)
          Stream defaultProtocol
  Network.Socket.bind sock (addrAddress serveraddr)
  listen sock 1
  -- only one connection open at a time
  conn <- open "finger.db"
  handleUpdates conn sock
  close conn
  close' sock
