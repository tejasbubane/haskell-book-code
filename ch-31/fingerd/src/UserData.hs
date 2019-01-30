{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module UserData where

import Database.SQLite.Simple hiding (close)
import Database.SQLite.Simple.Types
import Control.Exception
import Data.Text (Text)
import Data.Typeable
import Text.RawString.QQ

data User =
  User {
    userId :: Integer
  , username :: Text
  , shell :: Text
  , homeDirectory :: Text
  , realName :: Text
  , phone :: Text
  } deriving (Eq, Show)

instance FromRow User where
  fromRow = User <$> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field

instance ToRow User where
  toRow (User id' username' shell' homeDir' realName' phone') =
    toRow (id', username', shell', homeDir', realName', phone')

createUsers :: Query
createUsers = [r|
  CREATE TABLE IF NOT EXISTS users
    (id INTEGER PRIMARY KEY AUTOINCREMENT,
     username TEXT UNIQUE,
     shell TEXT,
     homeDirectory TEXT,
     realName TEXT, phone TEXT)
|]

-- Query is just newtype wrapper around Text
-- It has isString instance, so string literals can be Query values

insertQuery :: Query
insertQuery =
  "INSERT INTO users VALUES (?, ?, ?, ?, ?, ?)"

updateQuery :: Query
updateQuery =
  "UPDATE users SET (shell, homeDirectory, realName, phone) = (?, ?, ?, ?) where username = ?"

allQuery :: Query
allQuery =
  "SELECT * from users"

getUserQuery :: Query
getUserQuery =
  "SELECT * from users where username = ?"

data DuplicateData =
  DuplicateData
  deriving (Eq, Show, Typeable)

instance Exception DuplicateData

type UserRow = (Null, Text, Text, Text, Text, Text)
