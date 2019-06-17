module Main where

import           Cache.Redis                    ( mkRedisCache )
import           Config
import           Http.Api                       ( runServer )
import           Http.Client.Forex              ( mkForexClient )
import           Logger                         ( mkLogger )
import           Service.CachedForex            ( mkExchangeService )
import           Utils                          ( (>>>) )

main :: IO ()
main = do
  cfg     <- loadConfig >>> print
  cache   <- mkRedisCache $ redis cfg
  client  <- mkForexClient $ forex cfg
  logger  <- mkLogger
  service <- mkExchangeService logger cache client
  runServer service
