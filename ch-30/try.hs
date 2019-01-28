module Main where

import Control.Exception
import System.Environment (getArgs)

willFail :: Integer -> IO (Either ArithException ())
willFail denom =
  try $ print $ div 5 denom

-- Handle the error properly
handleArithException :: Show e => IO (Either e a) -> IO ()
handleArithException failure = do
  ex <- failure
  case ex of
    Left e  -> putStrLn $ show e
    Right x -> return ()

testDiv :: Integer -> IO ()
testDiv = handleArithException . willFail

main :: IO ()
main = do
  args <- getArgs
  mapM_ (testDiv . read) args

-- Compile and run passing arguments
-- ./try 1 2 0
