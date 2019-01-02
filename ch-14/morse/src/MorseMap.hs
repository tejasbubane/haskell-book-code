module MorseMap where

import Types
import qualified Data.Map as M

letterMorseMap :: (M.Map Char Morse)
letterMorseMap = M.fromList [
  ('a', ".-")
  , ('b', "-...")
  , ('c', "-.-.")
  , ('d', "-..")
  , ('e', ".")
  , ('f', "..-.")
  , ('g', "--.")
  , ('h', "....")
  , ('i', "..")
  , ('j', ".---")
  , ('k', "-.-")
  , ('l', ".-..")
  , ('m', "--")
  , ('n', "-.")
  , ('o', "---")
  , ('p', ".--.")
  , ('q', "--.-")
  , ('r', ".-.")
  , ('s', "...")
  , ('t', "-")
  , ('u', "..-")
  , ('v', "...-")
  , ('w', ".--")
  , ('x', "-..-")
  , ('y', "-.--")
  , ('z', "--..")
  , ('1', ".----")
  , ('2', "..---")
  , ('3', "...--")
  , ('4', "....-")
  , ('5', ".....")
  , ('6', "-....")
  , ('7', "--...")
  , ('8', "---..")
  , ('9', "----.")
  , ('0', "-----")
  ]

morseLetterMap :: M.Map Morse Char
morseLetterMap =
  M.foldrWithKey (flip M.insert) M.empty
                 letterMorseMap
