{-# LANGUAGE OverloadedStrings #-}

module Http.Routes
  ( rates
  )
where

import           Control.Monad.IO.Class         ( liftIO )
import           Domain
import           Servant
import           Servant.API
import           Service.CachedForex            ( ExchangeService(..)
                                                , mkExchangeService
                                                )

exchangeToResponse :: Currency -> Currency -> Exchange -> ExchangeResponse
exchangeToResponse from to rate = ExchangeResponse (getExchange rate) from to

rates
  :: ExchangeService
  -> Maybe Currency
  -> Maybe Currency
  -> Handler ExchangeResponse
rates service (Just from) (Just to) =
  liftIO $ exchangeToResponse from to <$> getRate service from to
rates _ _ _ = throwError $ err400 { errBody = "Invalid currencies" }
