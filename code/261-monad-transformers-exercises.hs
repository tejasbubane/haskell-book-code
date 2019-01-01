import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Identity

-- Chapter 26 Exercises

-- 1. `rDec` should get its argument in context of Reader and return a value
-- decremented by one.
rDec :: Num a => Reader a a
rDec = reader (subtract 1)

-- 2. pointfree `rDec` - already done

-- 3. `rShow` is show but in Reader.
rShow :: Show a => ReaderT a Identity String
rShow = ReaderT $ Identity . show

-- 4. Make `rShow` pointfree - already done

-- 5. `rPrintAndInc` will first print the input with a greeting, then
-- return the input incremented by one.
rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc =
  ReaderT $ \r -> do
    print (concat ["Hi: ", show r])
    return (r + 1)

-- 6. `sPrintIncAccum` first prints the input with greeting, then puts the
-- incremented input as new state and returns original input as String.
sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum =
  StateT $ \s -> do
    print (concat ["Hi: ", show s])
    return (show s, s + 1)

-- Fix the code
isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = do
  v <- lift getLine
  guard $ isValid v
  return v

doExcite :: IO ()
doExcite = do
  putStrLn "say something excite!"
  excite <- runMaybeT maybeExcite
  case excite of
    Nothing -> putStrLn "MOAR EXCITE"
    Just e -> putStrLn ("Good, was very excite: " ++ e)
