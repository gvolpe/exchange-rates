{-# LANGUAGE DataKinds, OverloadedStrings, TypeOperators #-}

module Http.Api (runServer) where

import           Data.Text
import           Data.Aeson
import           Data.Monoid                    ( (<>) )
import           Domain
import qualified Http.Routes                   as Routes
import           Servant
import           Servant.API
import           Service.CachedForex            ( ExchangeService(..) )
import           Network.Wai
import           Network.Wai.Handler.Warp

-- TODO: How to use a non-literal in the API definition?
v1 :: Text
v1 = "v1"

instance ToJSON Exchange
instance ToJSON Currency
instance FromJSON Currency

instance FromHttpApiData Currency where
  parseQueryParam x = case parseCurrency x of
    Just v  -> Right v
    Nothing -> Left $ "Invalid currency: " <> x

type ExchangeAPI =
       "v1" :> "rates" :> QueryParam "from" Currency :> QueryParam "to" Currency :> Get '[JSON] Exchange
  :<|> "v1" :> "currencies" :> Get '[JSON] [Currency]

exAPI :: Proxy ExchangeAPI
exAPI = Proxy

exchangeServer :: ExchangeService -> Server ExchangeAPI
exchangeServer s = Routes.rates s :<|> return currencies

api :: ExchangeService -> Application
api s = serve exAPI (exchangeServer s)

runServer :: ExchangeService -> IO ()
runServer s = do
  putStrLn "Started server on localhost:8080"
  run 8080 (api s)
