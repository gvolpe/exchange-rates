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
import           Control.Exception              ( bracket )
import           Data.Monoid                    ( (<>) )
import           Database.Redis                 ( Connection )
import           Domain.Currency                ( Currency )
import           Domain.Model                   ( Exchange )
import           GHC.Natural                    ( naturalToInteger )
import           Http.Client.Forex

newtype ExchangeService m = ExchangeService
  { getRate :: Currency -> Currency -> m Exchange
  }

mkExchangeService :: Cache IO -> ForexClient IO -> IO (ExchangeService IO)
mkExchangeService cache client =
  pure ExchangeService { getRate = getRate' cache client }

getRate' :: Cache IO -> ForexClient IO -> Currency -> Currency -> IO Exchange
getRate' cache client from to = cachedExchange cache from to >>= \case
  Just x  -> putStrLn ("Cache hit: " <> showEx from to) >> pure x
  Nothing -> do
    putStrLn $ "Calling web service for: " <> showEx from to
    bracket remoteCall cacheResult pure
   where
    exp         = expiration client
    remoteCall  = callForex client from to
    cacheResult = cacheNewResult cache exp from to

showEx :: Currency -> Currency -> String
showEx from to = show from <> " -> " <> show to
