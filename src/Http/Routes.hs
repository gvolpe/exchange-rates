{-# LANGUAGE OverloadedStrings #-}

module Http.Routes where

import           Control.Monad.IO.Class         ( liftIO )
import           Domain
import           Servant
import           Servant.API
import           Service.CachedForex            ( ExchangeService(..)
                                                , mkExchangeService
                                                )

rates :: ExchangeService -> Maybe Currency -> Maybe Currency -> Handler Exchange
rates service (Just from) (Just to) = liftIO $ getRate service from to
rates _ _ _ = throwError $ err400 { errBody = "Invalid currencies" }
