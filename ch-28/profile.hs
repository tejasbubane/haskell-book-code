module Main where

import Control.Monad

blah :: [Integer]
blah = [1..1000]

main :: IO ()
main =
  replicateM_ 10000 (print blah)

-- run using
-- ghc -prof -fprof-auto -rtsopts -O2 profile.hs
-- ./profile +RTS -hc -p
-- hp2ps profile.hp
-- open profile.ps
