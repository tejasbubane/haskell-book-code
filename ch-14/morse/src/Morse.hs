module Morse
    ( Morse
    , charToMorse
    , morseToChar
    , stringToMorse
    , morseToString
    ) where

import qualified Data.Map as M
import Types
import MorseMap

charToMorse :: Char -> Maybe Morse
charToMorse c =
  M.lookup c letterMorseMap

stringToMorse :: String -> Maybe [Morse]
stringToMorse s =
  sequence $ fmap charToMorse s

morseToChar :: Morse -> Maybe Char
morseToChar m =
  M.lookup m morseLetterMap

morseToString :: Morse -> Maybe String
morseToString line =
  traverse morseToChar (words line)
