module Main where

import Criterion.Main
import qualified Data.Sequence as S

lst :: [Int]
lst = [1..1000000]

sq :: S.Seq Int
sq = S.fromList [1..1000000]

main :: IO ()
main = defaultMain
  [ bench "concatenate lists"
    $ whnf (\xs -> xs !! 9001) lst
  , bench "concatenate sequences"
    $ whnf (flip S.index 9001) sq
  ]
