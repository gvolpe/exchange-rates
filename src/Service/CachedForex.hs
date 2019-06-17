{-# LANGUAGE BlockArguments, LambdaCase #-}

module Service.CachedForex
  ( ExchangeService(..)
  , mkExchangeService
  )
where

import           Cache.Redis                    ( Cache(..) )
import           Config                         ( ForexConfig
                                                , keyExpiration
                                                )
import           Context
import           Control.Exception              ( bracket )
import           Data.Monoid                    ( (<>) )
import           Database.Redis                 ( Connection )
import           Domain.Currency                ( Currency )
import           Domain.Model                   ( Exchange )
import           Logger                         ( Logger(..) )
import           GHC.Natural                    ( naturalToInteger )
import           Http.Client.Forex
import           RIO                     hiding ( bracket
                                                , logInfo
                                                )

newtype ExchangeService m = ExchangeService
  { getRate :: Currency -> Currency -> m Exchange
  }

mkExchangeService
  :: (HasLogger ctx, HasCache ctx, HasForexClient ctx)
  => RIO ctx (ExchangeService IO)
mkExchangeService = do
  logger <- view loggerL
  cache  <- view cacheL
  client <- view forexClientL
  pure ExchangeService { getRate = getRate' logger cache client }

getRate'
  :: Logger IO
  -> Cache IO
  -> ForexClient IO
  -> Currency
  -> Currency
  -> IO Exchange
getRate' l cache client from to = cachedExchange cache from to >>= \case
  Just x  -> logInfo l ("Cache hit: " <> showEx from to) >> pure x
  Nothing -> do
    logInfo l $ "Calling web service for: " <> showEx from to
    bracket remoteCall cacheResult pure
   where
    exp         = expiration client
    remoteCall  = callForex client from to
    cacheResult = cacheNewResult cache exp from to

showEx :: Currency -> Currency -> String
showEx from to = show from <> " -> " <> show to
