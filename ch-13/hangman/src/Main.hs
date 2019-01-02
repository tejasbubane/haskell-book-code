module Main where

import Data.Char (toLower)
import Words
import Game

main :: IO ()
main = do
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle
