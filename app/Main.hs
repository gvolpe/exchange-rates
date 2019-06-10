{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Config
import           Control.Applicative            ( (<|>) )
import           Control.Concurrent             ( threadDelay )
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Functor                   ( void )
import           Data.Monoid                    ( (<>) )
import           Forex                          ( showApiUsage
                                                , callForex
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
  showRates $ forex c

showRates :: ForexConfig -> IO ()
showRates c = void . keep' $ do
  var <- newEVar :: TransIO (EVar String)
  let f = rateLimiter (hours 1) $$(refineTH 1000) (callForex c) (lastWriteEVar var)
  let g = sleep 2 >> readEVar var >>= liftIO . print >> g :: TransIO ()
  let h = async (showApiUsage c) >> sleep 5 >> h :: TransIO ()
  f <|> g <|> h
  where sleep n = liftIO $ threadDelay (1000000 * n) :: TransIO ()
