{-# LANGUAGE OverloadedStrings #-}

module Http.Handler
  ( currencies
  , rates
  )
where

import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Catch            ( handle )
import           Domain.Currency                ( Currency )
import           Domain.Model                   ( Exchange(..) )
import           Http.Responses                 ( ExchangeResponse(..) )
import           Servant
import           Servant.API
import           Service.CachedForex            ( ApiLimitReachedException(..)
                                                , ExchangeService(..)
                                                , mkExchangeService
                                                )

import qualified Domain.Currency               as C

exchangeToResponse :: Currency -> Currency -> Exchange -> ExchangeResponse
exchangeToResponse from to rate = ExchangeResponse (getExchange rate) from to

rates
  :: ExchangeService IO
  -> Maybe Currency
  -> Maybe Currency
  -> Handler ExchangeResponse
rates service (Just from) (Just to) = handle
  (\ApiLimitReached ->
    throwError $ err503 { errBody = "Api limit has been reached" }
  )
  (liftIO $ exchangeToResponse from to <$> getRate service from to)
rates _ _ _ = throwError $ err400 { errBody = "Invalid currencies" }

currencies :: Handler [Currency]
currencies = pure C.currencies
