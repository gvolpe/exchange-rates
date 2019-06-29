{-# LANGUAGE BlockArguments, LambdaCase #-}

module Service.CachedForex
  ( ApiLimitReachedException(..)
  , ExchangeService(..)
  , mkExchangeService
  )
where

import           Config                         ( ForexConfig
                                                , apiKeyExpiration
                                                )
import           Context
import           Control.Lens                   ( view )
import           Control.Monad.Catch            ( Exception
                                                , MonadMask
                                                , bracket
                                                , finally
                                                , throwM
                                                )
import           Control.Monad.Reader.Class     ( MonadReader(..) )
import           Data.Functor                   ( (<&>) )
import           Data.Interface                 ( Cache(..)
                                                , Counter(..)
                                                , ExchangeService(..)
                                                )
import           Data.Monoid                    ( (<>) )
import           Database.Redis                 ( Connection )
import           Domain.Currency                ( Currency )
import           Domain.Model                   ( Exchange )
import           GHC.Natural                    ( naturalToInteger )
import           Http.Client.Forex              ( ForexClient(..) )
import           Logger                         ( Logger(..) )

mkExchangeService
  :: ( MonadMask m
     , HasLogger ctx m
     , HasCache ctx m
     , HasCounter ctx m
     , HasForexClient ctx m
     , MonadReader ctx r
     )
  => r (ExchangeService m)
mkExchangeService = do
  logger  <- view loggerL
  cache   <- view cacheL
  counter <- view counterL
  client  <- view forexClientL
  pure $ ExchangeService { getRate = getRate' logger counter cache client }

data ApiLimitReachedException = ApiLimitReached deriving Show

instance Exception ApiLimitReachedException

getRate'
  :: MonadMask m
  => Logger m
  -> Counter m
  -> Cache m
  -> ForexClient m
  -> Currency
  -> Currency
  -> m Exchange
getRate' l counter cache client from to =
  cachedExchange cache from to >>= \case
    Just x  -> logInfo l ("Cache hit: " <> showEx from to) >> pure x
    Nothing -> (getCount counter <&> (< limit)) >>= \case
      True -> do
        logInfo l $ "Calling web service for: " <> showEx from to
        bracket remoteCall cacheResult pure `finally` incrCount counter
      False -> throwM ApiLimitReached
     where
      exp         = expiration client
      limit       = reqPerHour client
      remoteCall  = callForex client from to
      cacheResult = cacheNewResult cache exp from to

showEx :: Currency -> Currency -> String
showEx from to = show from <> " -> " <> show to
