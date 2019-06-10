module Main where

import           Config
import           Control.Applicative            ( (<|>) )
import           Control.Concurrent             ( threadDelay )
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Functor                   ( void )
import           Data.Monoid                    ( (<>) )
import           Forex                          ( showApiUsage
                                                , showForex
                                                )
import           RateLimiter
import           Time
import           Transient.Base
import           Transient.Indeterminism

f :: ForexConfig -> (Int -> IO Int)
f c n = putStrLn ("Triggered #" <> show n) >> wait >> showApiUsage c >> pure n
  where wait = threadDelay (1000000 * 3)

main :: IO ()
main = do
  c <- loadConfig
  print c
  rateLimiter (hours 1) 100 (showForex $ forex c)

--showStuff :: ForexConfig -> IO ()
--showStuff c = void . keep' $ do
--  let f = rateLimiter (hours 1) 100 (showForex c)
--  let g = threadDelay (1000000 * 5) >> putStrLn "5 seconds..." >> g :: IO ()
--  rs <- (,) <$> async f <*> async g
--  liftIO $ print rs

  --results <- parTraverseN 4 (f $ forex c) [1 .. 10]
  --print results
