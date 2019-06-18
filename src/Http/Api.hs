{-# LANGUAGE DataKinds, OverloadedStrings, TypeOperators #-}

module Http.Api (runServer) where

import           Data.Text
import           Data.Aeson
import           Data.Monoid                    ( (<>) )
import           Domain.Currency
import qualified Http.Routes                   as Routes
import           Http.Responses
import           Servant
import           Servant.API
import           Service.CachedForex            ( ExchangeService(..) )
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Utils                          (maybeToEither)

type ApiVersion = "v1"

instance FromJSON Currency

instance FromHttpApiData Currency where
  parseQueryParam x =
    maybeToEither ("Invalid currency: " <> x) (parseCurrency x)

type RatesAPI =
       ApiVersion :> "rates" :> QueryParam "from" Currency :> QueryParam "to" Currency :> Get '[JSON] ExchangeResponse
  :<|> ApiVersion :> "currencies" :> Get '[JSON] [Currency]

ratesAPI :: Proxy RatesAPI
ratesAPI = Proxy

exchangeServer :: ExchangeService IO -> Server RatesAPI
exchangeServer s = Routes.rates s :<|> return currencies

api :: ExchangeService IO -> Application
api s = serve ratesAPI (exchangeServer s)

runServer :: ExchangeService IO -> IO ()
runServer s = do
  putStrLn "Started server on localhost:8080"
  run 8080 (api s)
