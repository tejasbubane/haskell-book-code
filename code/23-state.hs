{-# LANGUAGE InstanceSigs #-}

import System.Random
import Control.Applicative (liftA3)
import Control.Monad (replicateM, join)
import Control.Monad.Trans.State

data Die =
  DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    x -> error $ "intToDie got non 1-6 integer: " ++ show x

rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes = do
  let s = mkStdGen 0
      (d1, s1) = randomR (1, 6) s
      (d2, s2) = randomR (1, 6) s1
      (d3, _) = randomR (1, 6) s2
  (intToDie d1, intToDie d2, intToDie d3)
  -- this returns same result every time - because the initial seed (0) is same

rollDie :: State StdGen Die
rollDie = state $ do
  (n, s) <- randomR (1, 6)
  return (intToDie n, s)

rollDie' :: State StdGen Die
rollDie' =
  intToDie <$> state (randomR (1, 6))

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' =
  liftA3 (,,) rollDie rollDie rollDie

infiniteDie :: State StdGen [Die]
infiniteDie = repeat <$> rollDie
-- gives same result for entire list - we need to repeat `producing` of die
-- rather than single die value

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty g = go 0 0 g
  where go :: Int -> Int -> StdGen -> Int
        go sum count gen
          | sum >= 20 = count
          | otherwise =
            let (die, nextGen) = randomR (1, 6) gen
            in go (sum + die) (count + 1) nextGen

-- Exercise: Roll your own
-- 1. refactor  rollsToGetTwenty into having the limit be a function argument
rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n g = go 0 0 g
  where go :: Int -> Int -> StdGen -> Int
        go sum count gen
          | sum >= n = count
          | otherwise =
            let (die, nextGen) = randomR (1, 6) gen
            in go (sum + die) (count + 1) nextGen

-- 2. change rollsToGetN to recording the series of die that occurred
rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n g = go 0 0 [] g
  where go :: Int -> Int -> [Die] -> StdGen -> (Int, [Die])
        go sum count log gen
          | sum >= n = (count, log)
          | otherwise =
            let (die, nextGen) = randomR (1, 6) gen
            in go (sum + die) (count + 1) ((intToDie die):log) nextGen

-- State for Yourself
newtype Moi s a =
  Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi sa) = Moi $ \s ->
                            let (a, s') = sa s
                            in (f a, s')

instance Semigroup s => Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi $ \s -> (a, s)

  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (Moi sab) <*> (Moi sa) = Moi $ \s -> let (f, s') = sab s
                                           (a, s'') = sa s
                                       in (f a, s' <> s'')

instance Semigroup s => Monad (Moi s) where
  return = pure

  (>>=) :: Moi s a
        -> (a -> Moi s b)
        -> Moi s b
  (Moi sa) >>= g = Moi $ \s -> let (a, s') = sa s
                               in runMoi (g a) s

-- Fizzbuzz differently
-- using state just for fun
fizzbuzz :: Integer -> String
fizzbuzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 3 == 0  = "Fizz"
  | n `mod` 5 == 0  = "Buzz"
  | otherwise       = show n

addResult :: Integer -> State [String] ()
addResult n = do
  -- to remove ambiguity from `get` and `put` defined below
  xs <- Control.Monad.Trans.State.get
  let result = fizzbuzz n
  Control.Monad.Trans.State.put $ (fizzbuzz n):xs

fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list = execState (mapM_ addResult list) []

fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo n m = fizzbuzzList $ enumFromThenTo m (m - 1) n

-- Chapter Exercises
-- 1. state is also the value you return
get :: Moi s s
get = Moi $ \s -> (s, s)

-- 2. state is the argument provided and value is defaulted to unit
put :: s -> Moi s ()
put s = Moi $ \_ -> ((), s)

-- 3. Run state with s and get the state that results
exec :: Moi s a -> s -> s
exec (Moi sa) s = snd $ sa s

-- 4. Run state with s and get value that results
eval :: Moi s a -> s -> a
eval (Moi sa) s = fst $ sa s

-- 5. function which applies a function to create new State
modify :: (s -> s) -> Moi s ()
modify f = Moi $ \s -> ((), f s)
