module Main where

import           Cache.Redis                    ( mkRedisCache )
import           Config
import           Context                        ( Ctx(..) )
import           Http.Api                       ( runServer )
import           Http.Client.Forex              ( mkForexClient )
import           Logger                         ( mkLogger )
import           RIO                     hiding ( (>>>) )
import           Service.CachedForex            ( mkExchangeService )
import           Utils                          ( (>>>) )

main :: IO ()
main = do
  cfg    <- loadConfig >>> print
  cache  <- mkRedisCache $ redis cfg
  client <- mkForexClient $ forex cfg
  logger <- mkLogger
  let ctx = Ctx logger cache client
  service <- runRIO ctx mkExchangeService
  runServer service
