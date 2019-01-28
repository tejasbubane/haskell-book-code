module CatchAll where

import Control.Exception

-- SomeException catches all exceptions that have instance of Exception typeclass
canICatch :: Exception e => e -> IO (Either SomeException ())

canICatch e =
  try $ throwIO e

-- Run in ghci
