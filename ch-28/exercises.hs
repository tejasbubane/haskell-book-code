module Main where

import Criterion.Main

-- Difference List for cheap concat
newtype DList a = DL { unDL :: [a] -> [a] }
-- List represented as concat function
-- unDL ys = xs ++ ys

empty :: DList a
empty = DL id

singleton :: a -> DList a
singleton x = DL $ \xs -> (x:xs)

toList :: DList a -> [a]
toList (DL fxs) = fxs []

-- prepend single element
infixr `cons`
cons :: a -> DList a -> DList a
cons x xs = DL ((x:) . unDL xs)

-- append single element
infixl `snoc`
snoc :: DList a -> a -> DList a
snoc dxs x = DL ((++[x]) . unDL dxs)

append :: DList a -> DList a -> DList a
append (DL fxs) (DL fxs') = DL $ fxs . fxs'

normalList :: Int -> [Int]
normalList i = go i []
  where go 0 xs = xs
        go n xs = go (n - 1) ([n] ++ xs)

withDList :: Int -> [Int]
withDList i = toList $ go i empty
  where go 0 xs = xs
        go n xs = go (n - 1) (singleton n `append` xs)

-- A simple queue
data Queue a =
  Queue { enqueue :: [a]
        , dequeue :: [a]
        } deriving (Eq, Show)

qempty :: Queue a
qempty = Queue { enqueue = [], dequeue = [] }

balance :: Queue a -> Queue a
balance Queue { enqueue = back, dequeue = [] } =
  Queue { enqueue = [], dequeue = reverse back }
balance q = q

push :: a -> Queue a -> Queue a
push x (Queue { enqueue = back, dequeue = front }) =
  balance $ Queue { enqueue = (x:back), dequeue = front }

pop :: Queue a -> Maybe (a, Queue a)
pop Queue { dequeue = [] } = Nothing
pop Queue { enqueue = back, dequeue = (x:front) } =
  Just (x, balance $ Queue { enqueue = back, dequeue = front })

pushPopQueue :: Int -> Queue Int
pushPopQueue i = go i qempty
  where go 0 q = q
        go n q
          | (n `mod` 2) == 0 = go (n-1) (push 1 q)
          | otherwise        =
            case pop q of
              Nothing -> q
              Just (_, q') -> q'

pushL :: a -> [a] -> [a]
pushL x = (x:)

popL :: [a] -> Maybe (a, [a])
popL [] = Nothing
popL xs =
  let (y:ys) = reverse xs
  in Just (y, ys)

pushPopList :: Int -> [Int]
pushPopList i = go i []
  where go 0 q = q
        go n q
          | (n `mod` 2) == 0 = go (n-1) (pushL 1 q)
          | otherwise =
            case popL q of
              Nothing -> q
              Just (_, q') -> q'

-- Main
main :: IO ()
main = defaultMain
  [
    -- DList
    bench "concat list" $ whnf normalList 123456
  , bench "concat dlist" $ whnf withDList 123456

    -- Queue
  , bench "push-pop from list"
    $ whnf pushPopList 10000
  , bench "push-pop from okasaki queue"
    $ whnf pushPopQueue 10000
  ]
