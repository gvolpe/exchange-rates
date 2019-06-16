{-# LANGUAGE BlockArguments, LambdaCase #-}

module Service.CachedForex
  ( ExchangeService(..)
  , mkExchangeService
  )
where

import           Cache.Redis                    ( Cache(..)
                                                , Expiration(..)
                                                )
import           Config                         ( ForexConfig
                                                , keyExpiration
                                                )
import           Control.Exception              ( bracket )
import           Data.Monoid                    ( (<>) )
import           Database.Redis                 ( Connection )
import           Domain
import           GHC.Natural                    ( naturalToInt )
import           Http.Forex

newtype ExchangeService = ExchangeService
  { getRate :: Currency -> Currency -> IO Exchange
  }

mkExchangeService :: Cache -> ForexConfig -> IO ExchangeService
mkExchangeService cache cfg =
  pure ExchangeService { getRate = getRate' cache cfg }

getRate' :: Cache -> ForexConfig -> Currency -> Currency -> IO Exchange
getRate' c cfg from to = cachedExchange c from to >>= \case
  Just x  -> putStrLn ("Cache hit: " <> showEx from to) >> pure x
  Nothing -> do
    putStrLn $ "Calling web service for: " <> showEx from to
    bracket remoteCall cacheResult pure
   where
    exp = Expiration { getExpiration = naturalToInt $ keyExpiration cfg }
    remoteCall = callForex cfg from to
    cacheResult = cacheNewResult c exp from to

showEx :: Currency -> Currency -> String
showEx from to = show from <> " -> " <> show to
