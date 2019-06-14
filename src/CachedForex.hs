{-# LANGUAGE BlockArguments, LambdaCase #-}

module CachedForex
  ( exchangeRate
  )
where

import           Cache
import qualified Config                        as C
import           Control.Exception              ( bracket )
import           Data.Monoid                    ( (<>) )
import           Domain
import           Forex

showEx :: Currency -> Currency -> String
showEx from to = show from <> " -> " <> show to

exchangeRate :: C.AppConfig -> Currency -> Currency -> IO Exchange
exchangeRate c from to = cachedExchange (C.redis c) from to >>= \case
  Just x  -> putStrLn ("Cache hit: " <> showEx from to) >> pure x
  Nothing -> do
    putStrLn $ "Calling web service for: " <> showEx from to
    bracket remoteCall cacheResult pure
   where
    remoteCall  = callForex (C.forex c) from to
    cacheResult = cacheNewResult (C.redis c) from to
