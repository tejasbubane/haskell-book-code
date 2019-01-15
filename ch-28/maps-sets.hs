module Main where

import Criterion.Main
import qualified Data.Map as M
import qualified Data.Set as S

genList :: Int -> Int -> [(String, Int)]
genList x y = go y []
  where go n xs
          | n == x     = (show n, n) : xs
          | otherwise  = go (n - 1) ((show n, n) : xs)

assocList :: [(String, Int)]
assocList = genList 0 10000

anotherList :: [(String, Int)]
anotherList = genList 10001 20000

myMap :: M.Map String Int
myMap = M.fromList assocList

anotherMap :: M.Map String Int
anotherMap = M.fromList anotherList

mySet :: S.Set String
mySet = S.fromList $ map fst assocList

anotherSet :: S.Set String
anotherSet = S.fromList $ map fst anotherList

main :: IO ()
main = defaultMain
  [ -- lookups
    bench "lookup in assoc-list"
    $ whnf (lookup "doesn't exist") assocList
  , bench "lookup in map"
    $ whnf (M.lookup "doesn't exist") myMap
  , bench "lookup in set"
    $ whnf (S.member "doesn't exist") mySet

    -- intersection
  , bench "intersection of maps"
    $ whnf (M.intersection myMap) anotherMap
  , bench "intersection of sets"
    $ whnf (S.intersection mySet) anotherSet

    -- union
  , bench "union of maps"
    $ whnf (M.union myMap) anotherMap
  , bench "union of sets"
    $ whnf (S.union mySet) anotherSet
  ]
-- Maps are way faster than association lists
-- Association lists are fine for cheap maps, but for anything practical use Map
