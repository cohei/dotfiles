{-# LANGUAGE ViewPatterns #-}
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [read -> r, read -> n] -> print $ paymentRatio r n
    _ -> putStr usage

usage :: String
usage =
  unlines
  [ "NAME"
  , "     level-payment - ratio of payment to principal"
  , ""
  , "SYNOPSIS"
  , "     level-payment interest number-of-payments"
  ]

-- | ratio of payment to principal
paymentRatio
  :: Double -- ^ interest
  -> Int    -- ^ number of payments
  -> Double
paymentRatio r n = r / (1 - (1 + r) ^^ (- n))
