{-# LANGUAGE OverloadedStrings #-}

module Forex
  ( callForex
  , showApiUsage
  )
where

import qualified Config                        as C
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Map                       ( Map)
import qualified Data.Map                      as M
import           Data.Monoid                    ( (<>) )
import           Data.Text
import           Domain
import           GHC.Generics (Generic)
import           Network.Wreq

newtype Exchange = Exchange { value :: Float } deriving Show

instance FromJSON Exchange where
  parseJSON v = do
   j <- parseJSON v :: Parser (Map Text Value)
   case M.toList j of
     [(_, x)] -> Exchange <$> (parseJSON x :: Parser Float)

callForex :: C.ForexConfig -> Currency -> Currency -> IO Exchange
callForex c from to = makeReq ops url
 where
  makeReq ops url =
    (^. responseBody) <$> (asJSON =<< getWith ops url :: IO (Response Exchange))
  url = unpack (C.apiHost c <> C.apiPath c <> "/convert")
  exc = [pack $ show from <> "_" <> show to]
  ops = defaults & param "q" .~ exc & param "compact" .~ ["ultra"] & param "apiKey" .~ [C.apiKey c]

showApiUsage :: C.ForexConfig -> IO ()
showApiUsage c = makeReq ops url
 where
  makeReq ops url = do
    r <- getWith ops url
    print $ r ^. responseBody
  url = unpack (C.apiHost c <> C.apiUsage c)
  ops = defaults & param "apiKey" .~ [C.apiKey c]
