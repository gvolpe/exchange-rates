module Main where

import           Cache.Redis                    ( mkRedisCache )
import           Config
import           Http.Api                       ( runServer )
import           Service.CachedForex            ( mkExchangeService )
import           Utils                          ( (>>>) )

main :: IO ()
main = do
  cfg     <- loadConfig >>> print
  cache   <- mkRedisCache $ redis cfg
  service <- mkExchangeService cache (forex cfg)
  runServer service
