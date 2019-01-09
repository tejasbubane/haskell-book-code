{-# LANGUAGE Strict #-} -- Make entire module strict by default

module StrictTest where

blah x = 1

-- everything is Strict
-- this fails even if function does not use arg
blahTest :: IO ()
blahTest = print (blah undefined)

-- bring back laziness if needed
willNotForce :: Int -> Int
willNotForce ~x = 1
-- passing undefined to this func will work - even within Strict module

-- Chapter Exercises
data List a =
    Nil
  | Cons ~a ~(List a)
  deriving (Show)

take' n _ | n <= 0 = Nil
take' _ Nil = Nil
take' n (Cons x xs) = Cons x $ take' (n - 1) xs

map' _ Nil = Nil
map' f (Cons x xs) = Cons (f x) $ map' f xs

repeat' x = xs where xs = (Cons x xs)

main = do
  print $ take' 10 $ map' (+1) (repeat' 1)
-- adding ~ here makes the main work otherwise it goes in infinite loop with strict repeat
