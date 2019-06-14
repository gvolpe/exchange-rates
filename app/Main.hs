{-# LANGUAGE TemplateHaskell #-}

module Main where

import           CachedForex                    ( exchangeRate )
import           Config
import           Control.Applicative            ( (<|>) )
import           Control.Concurrent             ( threadDelay )
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Functor                   ( void )
import           Data.Monoid                    ( (<>) )
import           Data.Foldable                  ( traverse_ )
import           Domain
import           Forex                          ( callForex
                                                , getApiUsage
                                                )
import           RateLimiter
import           Refined
import           Time
import           Transient.Base
import           Transient.EVars
import           Transient.Indeterminism

main :: IO ()
main = do
  c <- loadConfig
  print c
  (getApiUsage $ forex c) >>= print
  let rates = [(USD, ARS), (EUR, PLN), (USD, ARS), (EUR, GBP), (EUR, PLN)]
  traverse_ (\(from, to) -> exchangeRate c from to >>= print) rates
  (getApiUsage $ forex c) >>= print

showRates :: ForexConfig -> IO ()
showRates c = void . keep' $ do
  var <- newEVar :: TransIO (EVar Exchange)
  let f = rateLimiter (hours 1) $$(refineTH 1000) (callForex c USD PLN) (lastWriteEVar var)
  let g = sleep 2 >> readEVar var >>= liftIO . print >> g :: TransIO ()
  let h = async (getApiUsage c >>= print) >> sleep 5 >> h :: TransIO ()
  f <|> g <|> h
  where sleep n = liftIO $ threadDelay (1000000 * n) :: TransIO ()
