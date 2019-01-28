module CustomExceptions where

import Control.Exception

data NotDivFive =
  NotDivFive Int
  deriving (Eq, Show)

instance Exception NotDivFive

data NotEven =
  NotEven Int
  deriving (Eq, Show)

instance Exception NotEven

evenAndFiveDiv :: Int -> IO Int
evenAndFiveDiv i
  | rem i 5 /= 0 = throwIO $ NotDivFive i -- Custom errors can take context
  | odd i = throwIO $ NotEven i
  | otherwise = return i

-- We can also related custom exceptions together
data InvalidNumberException =
  NotEven' Int
  | NotDivFive' Int
  deriving (Eq, Show)

instance Exception InvalidNumberException

-- Since this is now Sum type, we can define one handler function
-- and handle both using pattern match
