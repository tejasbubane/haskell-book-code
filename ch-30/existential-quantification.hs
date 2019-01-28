{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

module WhySomeException where

import Control.Exception (ArithException(..), AsyncException(..))
import Data.Typeable

data MyException =
  forall e . (Show e, Typeable e) => MyException e

instance Show MyException where
  showsPrec p (MyException e) = showsPrec p e

multiError :: Int -> Either MyException Int
multiError n =
  case n of
    0 -> Left (MyException DivideByZero)
    1 -> Left (MyException StackOverflow)
    _ -> Right n
-- We can use different exception with MyException because of the existential quantification
-- Note: MyException does not take any type variable
-- This is why we need existential quantification - to throw various exception types
-- without being forced to centralize all error types under single SUM type
