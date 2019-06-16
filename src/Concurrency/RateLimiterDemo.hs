{-# LANGUAGE TemplateHaskell #-}

module RateLimiterDemo where

import           Concurrency.RateLimiter
import           Config
import           Control.Applicative            ( (<|>) )
import           Control.Concurrent             ( threadDelay )
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Functor                   ( void )
import           Domain
import           Http.Api                       ( runServer )
import           Http.Forex                     ( callForex
                                                , getApiUsage )
import           Refined
import           Time
import           Transient.Base
import           Transient.EVars
import           Transient.Indeterminism

showRates :: ForexConfig -> IO ()
showRates c = void . keep' $ do
  var <- newEVar :: TransIO (EVar Exchange)
  let f = rateLimiter (hours 1) $$(refineTH 1000) (callForex c USD PLN) (lastWriteEVar var)
  let g = sleep 2 >> readEVar var >>= liftIO . print >> g :: TransIO ()
  let h = async (getApiUsage c >>= print) >> sleep 5 >> h :: TransIO ()
  f <|> g <|> h
  where sleep n = liftIO $ threadDelay (1000000 * n) :: TransIO ()
