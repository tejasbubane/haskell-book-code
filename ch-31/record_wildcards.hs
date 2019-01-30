{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module RWCDemo where

data Blah =
  Blah { myThing :: Int }
  deriving (Eq, Show)

wew Blah{..} = print myThing
