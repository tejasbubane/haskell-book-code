module Main where

import System.Environment (getArgs)
import Data.Char (ord, chr)
import System.IO (stdin, stdout, stderr, hGetLine, hPutStr, hWaitForInput)

-- Since nothing mentioned about keyword in the exercise description
-- using my own hardcoded keyword
keyword :: String
keyword = "haskell"

alphaIndex :: Char -> Int
alphaIndex x = ord x - 97

bound :: Int -> Int
bound i
  | i > upperBound = lowerBound + (i - upperBound) - 1
  | i < lowerBound = upperBound - (lowerBound - i) + 1
  | otherwise      = i
  where lowerBound = ord 'a'
        upperBound = ord 'z'

-- Vigenère cipher
-- Assumption input is smallcase alphabets only
encrypt :: String -> String
encrypt xs = map encryptChar $ zip xs (cycle keyword)
  where encryptChar (x, k) = chr . bound $ ord x + alphaIndex k

decrypt :: String -> String
decrypt xs = map decryptChar $ zip xs (cycle keyword)
  where decryptChar (x, k) = chr . bound $ ord x - alphaIndex k

process :: [String] -> IO ()
process (op:_)
  | op == "-d" = hGetLine stdin >>= ((hPutStr stdout) . decrypt)
  | op == "-e" = hGetLine stdin >>= ((hPutStr stdout) . encrypt)

main :: IO ()
main = do
  (op:ops) <- getArgs
  if op == "-t" -- Assuming first option will be -t followed by -e or -d
  then do
    r <- hWaitForInput stdin (read (head ops) :: Int)
    if r then process (tail ops) else hPutStr stderr "Read Timeout"
  else process (op:ops)

-- Usage:

-- Compile so that we can pass the CLI arguments
-- ghc exercises.hs

-- Run the generated executable (timeout in milliseconds)
-- ./exercises -t 5000 -e
-- ./exercises -t 5000 -d

-- or without the -t option
-- ./exercises -e
-- ./exercises -d

-- Skipping the Config Directories exercise
