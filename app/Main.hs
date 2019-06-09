module Main where

import           Config
import           Control.Concurrent             ( threadDelay )
import           Data.Monoid                    ( (<>) )
import           Forex                          ( showApiUsage
                                                , showForex
                                                )
import           RateLimiter                    ( rateLimiter )

wait :: IO ()
wait = threadDelay (1000000 * 5)

main :: IO ()
main = do
  c <- loadConfig
  print c
  rateLimiter 2 $ map
    (\n -> putStrLn ("Triggered #" <> show n) >> wait >> showApiUsage (forex c))
    [1 .. 10]
