{-# LANGUAGE BangPatterns #-}

foldr' k z xs = go xs
  where
    go []     = z
    go (y:ys) = y `k` go ys

hypo :: IO ()
hypo = do
  let x :: Int
      x = undefined -- in non-strict langs, this will be forced before binding
  s <- getLine
  case s of
    "hi" -> print x
    _    -> putStrLn "hello"
-- but in non-strict langs like haskell x is bound but not forced evaluation
-- until required - hence it works as long as input is not "hi"

-- Strict version of above function
hypo' :: IO ()
hypo' = do
  let x :: Int
      x = undefined
  s <- getLine
  case x `seq` s of
    "hi" -> print x
    _    -> putStrLn "hello"
-- `seq forces evaluation of first arg when second is evaluated.
-- Since `s` is evaluated every time - no matter what you enter now -
-- `x` will also be evaluated leading the program to fail.
-- Still it really is not truly a strict lang because getLine was evaluated before `x`

hypo'' :: IO ()
hypo'' = do
  let x :: Int
      x = undefined
  s <- x `seq` getLine -- force x before getLine
  -- This^ is what real strict languages would do
  case s of
    _ -> putStrLn "doesn't matter"

notGonnaHappenBru :: Int
notGonnaHappenBru =
  let x = undefined
      y = 2
      z = (x `seq` y `seq` 10, 11)
  in snd z
-- snd returns 11 - so fst never evaluated

doesntEval :: Bool -> Int
doesntEval b = 1

manualSeq :: Bool -> Int
manualSeq b = b `seq` 1

banging :: Bool -> Int
banging !b = 1

-- Making lazy lists strict
data List a =
    Nil
  | Cons !a !(List a)
  deriving Show

sTake :: Int -> List a -> List a
sTake n _
  | n <= 0 = Nil
sTake n Nil         = Nil
sTake n (Cons a xs) = Cons a $ sTake (n - 1) xs

twoEls = Cons 1 (Cons undefined Nil)
oneEl  = sTake 1 twoEls
threeEls = Cons 2 twoEls
oneElT = sTake 1 threeEls

-- Chapter Exercise
-- Make expression bottom - using only bang patterns or seq
x = undefined
y = "blah"
main = do
  print $ snd (x, x `seq` y)
