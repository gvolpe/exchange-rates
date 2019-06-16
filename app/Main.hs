{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Cache.Redis                    ( mkRedisCache )
import           Concurrency.RateLimiter
import           Config
import           Control.Applicative            ( (<|>) )
import           Control.Concurrent             ( threadDelay )
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Functor                   ( void )
import           Data.Monoid                    ( (<>) )
import           Data.Foldable                  ( traverse_ )
import           Domain
import           Http.Api                       ( runServer )
import           Http.Forex                     ( callForex
                                                , getApiUsage
                                                )
import           Refined
import           Service.CachedForex            ( ExchangeService(..)
                                                , mkExchangeService
                                                )
import           Time
import           Transient.Base
import           Transient.EVars
import           Transient.Indeterminism
import           Utils                          ( (>>>) )

main :: IO ()
main = do
  c <- loadConfig >>> print
  cache   <- mkRedisCache $ redis c
  service <- mkExchangeService cache (forex c)
  runServer service

main1 :: IO ()
main1 = do
  c <- loadConfig >>> print
  let fc = forex c
  cache   <- mkRedisCache $ redis c
  service <- mkExchangeService cache fc
  showApiUsage fc
  traverse_ (\(from, to) -> getRate service from to >>= print) rates
  showApiUsage fc
 where
  showApiUsage fc = getApiUsage fc >>= print
  rates = [(USD, ARS), (EUR, PLN), (USD, ARS), (EUR, GBP), (EUR, PLN)]

showRates :: ForexConfig -> IO ()
showRates c = void . keep' $ do
  var <- newEVar :: TransIO (EVar Exchange)
  let f = rateLimiter (hours 1)
                      $$(refineTH 1000)
                      (callForex c USD PLN)
                      (lastWriteEVar var)
  let g = sleep 2 >> readEVar var >>= liftIO . print >> g :: TransIO ()
  let h = async (getApiUsage c >>= print) >> sleep 5 >> h :: TransIO ()
  f <|> g <|> h
  where sleep n = liftIO $ threadDelay (1000000 * n) :: TransIO ()
