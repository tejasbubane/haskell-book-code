module Main where

import Control.Exception
import Data.Typeable

-- show error message and die
handler :: SomeException -> IO ()
handler (SomeException e) = do
  print (typeOf e)
  putStrLn ("We errored! It was: " ++ show e)

-- show error message and write to alternate file (bbb) instead
betterHandler :: SomeException -> IO ()
betterHandler (SomeException e) = do
  putStrLn ("Running main caused an error! It was: " ++ show e)
  writeFile "bbb" "hi"

-- Write to a file (zzz) with no permissiosn
main =
  writeFile "zzz" "hi"
    `catch` betterHandler

-- Compile and run ./catch
