{-# LANGUAGE BlockArguments, LambdaCase, RecordWildCards #-}

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
getRate' Logger {..} Counter {..} Cache {..} ForexClient {..} from to =
  cachedExchange from to >>= \case
    Just x  -> logInfo ("Cache hit: " <> showEx from to) >> pure x
    Nothing -> (getCount <&> (< reqPerHour)) >>= \case
      True -> do
        logInfo $ "Calling web service for: " <> showEx from to
        bracket remoteCall cacheResult pure `finally` incrCount
      False -> throwM ApiLimitReached
     where
      remoteCall  = callForex from to
      cacheResult = cacheNewResult expiration from to

showEx :: Currency -> Currency -> String
showEx from to = show from <> " -> " <> show to
