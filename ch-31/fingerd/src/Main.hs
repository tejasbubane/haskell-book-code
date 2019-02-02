module Main where

import Control.Concurrent (forkIO)
import FingerServer (fingerServer)
import UserServer (userServer)

main :: IO ()
main = do
  putStrLn "Starting finger server on port 79"
  _ <- forkIO $ fingerServer "79"

  putStrLn "Starting user update server on port 12345"
  _ <- forkIO $ userServer "12345"

  putStrLn "Waiting for input.. Type return to exit"
  _ <- getLine
  putStrLn "Stopping.."
