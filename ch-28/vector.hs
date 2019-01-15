module Main where

import Criterion.Main
import Data.Vector ((//))
import qualified Data.Vector as V

slice :: Int -> Int -> [a] -> [a]
slice from len = take len . drop from

l :: [Int]
l = [1..10000]

v :: V.Vector Int
v = V.fromList l

testV' :: Int -> V.Vector Int
testV' n =
  V.map (+n) $ V.map (+n) $
    V.map (+n) $ V.map (+n)
    (V.fromList [1..10000])

testV :: Int -> V.Vector Int
testV n =
  V.map ((+n) . (+n) . (+n) . (+n)) (V.fromList [1..10000])

slow :: Int -> V.Vector Int
slow n = go n v
  where go 0 v = v
        go n v = go (n-1) (v // [(n,0)])

-- batching all updates once is way faster than doing it one by one
batchList :: Int -> V.Vector Int
batchList n = v // updates
  where updates = fmap (\n -> (n, 0)) [0..n]

-- unsafeUpdate takes vector of updates
-- this is slightly faster than list of updates
batchList' :: Int -> V.Vector Int
batchList' n = V.unsafeUpdate v updates
  where updates = fmap (\n -> (n, 0)) (V.fromList [0..n])

main :: IO ()
main = defaultMain
  [ -- slicing
    bench "slicing list"
    $ whnf (head . slice 100 9000) l
  , bench "slicing vector"
    -- slicing is very fast with vectors compared to lists
    $ whnf (V.head . V.slice 100 9000) v

    -- random access
  , bench "finding nth element in vector without slicing"
    $ whnf ((V.!) v) 100

    -- Fusion
  , bench "vector map prefused"
  $ whnf testV 9998
  , bench "vector map unfused"
    -- vector lib has loop fusion built-in
    -- so this will also be fused while compilation
  $ whnf testV' 9998

    -- batch updates
  , bench "one by one update"
  $ whnf slow 9998
  , bench "batch update with list of updates"
  $ whnf batchList 9998
  , bench "batch update with vector of updates"
  $ whnf batchList' 9998
  ]
