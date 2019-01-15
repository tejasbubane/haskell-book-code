module NestedIO where

import Data.Time.Calendar
import Data.Time.Clock
import System.Random

huehue :: IO (Either (IO Int) (IO ()))
huehue = do
  t <- getCurrentTime
  let (_, _, dayOfMonth) =
        toGregorian (utctDay t)
  case even dayOfMonth of
    True  -> return $ Left randomIO
    False -> return $ Right (putStrLn "not even")
-- Note the IO action returned is dependent on having performed other effectful
-- action of getting current time - this is not possible with Applicatives
