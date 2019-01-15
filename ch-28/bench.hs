module Main where

import Criterion.Main

infixl 9 !?
(!?) :: [a] -> Int -> Maybe a
_ !? n | n < 0 = Nothing
[] !? _        = Nothing
(x:_) !? 0     = Just x
(x:xs) !? n    = xs !? (n - 1)

-- faster version - based on !! implementation in `base`
infixl 9 !?!
{-# INLINABLE (!?!) #-}
(!?!) :: [a] -> Int -> Maybe a
xs !?! n
  | n < 0     = Nothing
  | otherwise =
      foldr (\x r k -> case k of
                         0 -> Just x
                         _ -> r (k - 1)) (const Nothing) xs n


myList :: [Int]
myList = [1..9999]

myList' :: [Int]
myList' = [1..9999] ++ [undefined]

main :: IO ()
main = defaultMain
  [ bench "index list 9999"
    $ whnf (myList !!) 9998
  , bench "index list maybe index 9999"
    $ whnf (myList !?) 9998
  , bench "index list maybe refactored 9999"
    $ whnf (myList !?!) 9998
  , bench "index benchmark which fails"
    -- access last element which is undefined
    -- chaging whnf` to `nf` forces evaluation of whats inside Maybe - and fails
    $ whnf (myList' !?) 9999
  , bench "map list 9999"
    $ nf (map (+1)) myList
  ]
