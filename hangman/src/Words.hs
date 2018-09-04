module Words where

import System.Random (randomRIO)

newtype WordList =
  WordList [String]
  deriving (Eq, Show)

-- read all words from dictionary
allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

-- filter words within length limits
gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
  where gameLength w =
          let l = length w
          in l >= minWordLength && l <= maxWordLength

-- return random element from given list
randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, (length wl) - 1)
  return $ wl !! randomIndex

-- call above function with gameWords
randomWord' :: IO String
randomWord' = gameWords >>= randomWord
