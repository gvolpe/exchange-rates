module Main where

import           Cache.Redis                    ( mkRedisCache )
import           Config
import           Http.Api                       ( runServer )
import           Http.Client.Forex              ( mkForexClient )
import           Service.CachedForex            ( mkExchangeService )
import           Utils                          ( (>>>) )

main :: IO ()
main = do
  cfg     <- loadConfig >>> print
  cache   <- mkRedisCache $ redis cfg
  client  <- mkForexClient $ forex cfg
  service <- mkExchangeService cache client
  runServer service
