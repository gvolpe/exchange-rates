module Main where

import           Config
import           Control.Concurrent             ( threadDelay )
import           Data.Monoid                    ( (<>) )
import           Forex                          ( showApiUsage
                                                , showForex
                                                )
import           RateLimiter                    ( parTraverseN )

f :: ForexConfig -> (Int -> IO Int)
f c n = putStrLn ("Triggered #" <> show n) >> wait >> showApiUsage c >> pure n
  where wait = threadDelay (1000000 * 3)

main :: IO ()
main = do
  c <- loadConfig
  print c
  results <- parTraverseN 4 (f $ forex c) [1 .. 10]
  print results
