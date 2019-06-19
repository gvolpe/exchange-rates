module Main where

import           Cache.Redis                    ( mkRedisCache )
import           Config
import           Context
import           Http.Api                       ( runServer )
import           Http.Client.Forex              ( mkForexClient )
import           Logger                         ( defaultLogger )
import           RIO                     hiding ( (>>>) )
import           Service.CachedForex            ( mkExchangeService )
import           Utils                          ( (>>>) )

mkContext :: RIO Env (Ctx IO)
mkContext = do
  cache <- mkRedisCache
  Ctx defaultLogger cache <$> mkForexClient

main :: IO ()
main = do
  env     <- Env <$> loadConfig >>> print
  ctx     <- runRIO env mkContext
  service <- runRIO ctx mkExchangeService
  runServer service
