module Main (main) where

import           Data.Time          (DiffTime, defaultTimeLocale, formatTime,
                                     parseTimeOrError)
import           System.Environment (getArgs)

main :: IO ()
main = putStrLn . showTime . sum . map readTime =<< getArgs

timeFormat :: String
timeFormat = "%h:%0M:%0S"

readTime :: String -> DiffTime
readTime ('-' : s) = - readPositiveTime s
readTime s         =   readPositiveTime s

readPositiveTime :: String -> DiffTime
readPositiveTime = parseTimeOrError False defaultTimeLocale timeFormat

showTime :: DiffTime -> String
showTime t
  | negative t = "-" <> showPositiveTime (abs t)
  | otherwise  = showPositiveTime t

showPositiveTime :: DiffTime -> String
showPositiveTime = formatTime defaultTimeLocale timeFormat

negative :: (Ord a, Num a) => a -> Bool
negative = (< 0)
