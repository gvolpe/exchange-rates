{-# LANGUAGE BlockArguments, LambdaCase #-}

module Service.CachedForex
  ( exchangeRate
  )
where

import           Cache.Cache                    ( RedisCache(..) )
import           Config                         ( ForexConfig )
import           Control.Exception              ( bracket )
import           Data.Monoid                    ( (<>) )
import           Database.Redis                 ( Connection )
import           Domain
import           Http.Forex

showEx :: Currency -> Currency -> String
showEx from to = show from <> " -> " <> show to

exchangeRate :: RedisCache -> ForexConfig -> Currency -> Currency -> IO Exchange
exchangeRate c cfg from to = cachedExchange c from to >>= \case
  Just x  -> putStrLn ("Cache hit: " <> showEx from to) >> pure x
  Nothing -> do
    putStrLn $ "Calling web service for: " <> showEx from to
    bracket remoteCall cacheResult pure
   where
    remoteCall  = callForex cfg from to
    cacheResult = cacheNewResult c from to
