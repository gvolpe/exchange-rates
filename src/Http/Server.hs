{-# LANGUAGE DataKinds, OverloadedStrings, TypeOperators #-}

module Http.Server (runServer) where

import           Control.Lens
import           Data.Text
import           Data.Aeson
import           Data.Monoid                    ( (<>) )
import           Data.Proxy
import           Data.Swagger
import           Domain.Currency
import qualified Http.Handler                   as Handler
import           Http.Responses
import           Servant
import           Servant.API
import           Servant.Swagger
import           Service.CachedForex            ( ExchangeService(..) )
import           Network.Wai                    ( Application )
import           Network.Wai.Handler.Warp       ( run )
import           Network.Wai.Middleware.Cors    ( simpleCors )
import           Utils                          ( maybeToEither )

type ApiVersion = "v1"

instance FromJSON Currency
instance ToParamSchema Currency
instance ToSchema Currency
instance ToSchema ExchangeResponse

instance FromHttpApiData Currency where
  parseQueryParam x =
    maybeToEither ("Invalid currency: " <> x) (parseCurrency x)

type SwaggerAPI = "swagger.json" :> Get '[JSON] Swagger

type RatesAPI =
       ApiVersion :> "rates" :> QueryParam "from" Currency :> QueryParam "to" Currency :> Get '[JSON] ExchangeResponse
  :<|> ApiVersion :> "currencies" :> Get '[JSON] [Currency]

apiSwagger :: Swagger
apiSwagger = toSwagger (Proxy :: Proxy RatesAPI)
  & info.title   .~ "Exchange Rates API"
  & info.version .~ "1.0"
  & info.description ?~ "Exchange rates for everyone!"
  & info.license ?~ ("Apache 2" & url ?~ URL "https://www.apache.org/licenses/LICENSE-2.0")

type API = SwaggerAPI :<|> RatesAPI

exchangeServer :: ExchangeService IO -> Server API
exchangeServer s = return apiSwagger :<|> Handler.rates s :<|> Handler.currencies

api :: ExchangeService IO -> Application
api s = serve (Proxy :: Proxy API) (exchangeServer s)

runServer :: ExchangeService IO -> IO ()
runServer s = do
  putStrLn "Started server on localhost:8080"
  run 8080 $ simpleCors (api s)
