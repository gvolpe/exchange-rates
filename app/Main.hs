module Main where

import           Cache.Redis                    ( mkRedisCache )
import           Config
import           Context
import           Http.Client.Forex              ( mkForexClient )
import           Http.Server                    ( runServer )
import           Logger                         ( defaultLogger )
import           RIO
import           Service.CachedForex            ( mkExchangeService )
import           Utils                          ( tap )

mkContext :: RIO Env (Ctx IO)
mkContext = do
  cache <- mkRedisCache
  Ctx defaultLogger cache <$> mkForexClient

main :: IO ()
main = do
  env     <- Env <$> loadConfig `tap` print
  ctx     <- runRIO env mkContext
  service <- runRIO ctx mkExchangeService
  runServer service
