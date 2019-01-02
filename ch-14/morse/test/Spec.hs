module Main where

import qualified Data.Map as M
import Morse
import MorseMap
import Test.QuickCheck

allowedChars :: [Char]
allowedChars = M.keys letterMorseMap

allowedMorse :: [Morse]
allowedMorse = M.keys morseLetterMap

charGen :: Gen Char
charGen = elements allowedChars

morseGen :: Gen Morse
morseGen = elements allowedMorse

morseAndBackAgain :: Property
morseAndBackAgain =
  forAll charGen
  (\c -> ((charToMorse c) >>= morseToChar) == Just c)

charAndBackAgain :: Property
charAndBackAgain =
  forAll morseGen
  (\m -> ((morseToChar m) >>= charToMorse) == Just m)

main :: IO ()
main = do
  quickCheck morseAndBackAgain
  quickCheck charAndBackAgain
