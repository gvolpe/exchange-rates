{-# LANGUAGE OverloadedStrings, RankNTypes, ScopedTypeVariables #-}

module Http.Forex
  ( callForex
  , getApiUsage
  )
where

import qualified Config                        as C
import           Control.Lens
import           Data.Aeson              hiding ( Options )
import           Data.Aeson.Types        hiding ( Options )
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.Monoid                    ( (<>) )
import           Data.Text
import           Domain
import           GHC.Generics                   ( Generic )
import           Network.Wreq

instance FromJSON Exchange where
  parseJSON v = do
    j <- parseJSON v :: Parser (Map Text Value)
    case M.toList j of
      [(_, x)] -> Exchange <$> (parseJSON x :: Parser Float)

instance FromJSON ApiUsage

callForex :: C.ForexConfig -> Currency -> Currency -> IO Exchange
callForex c from to =
  let url = C.apiHost c <> C.apiPath c <> "/convert"
      exc = param "q" .~ [pack $ show from <> "_" <> show to]
      key = param "apiKey" .~ [C.apiKey c]
      ops = defaults & exc & param "compact" .~ ["ultra"] & key
  in  req ops url

getApiUsage :: C.ForexConfig -> IO ApiUsage
getApiUsage c =
  let url = C.apiHost c <> C.apiUsage c
      ops = defaults & param "apiKey" .~ [C.apiKey c]
  in  req ops url

req :: forall a . FromJSON a => Options -> Text -> IO a
req ops url =
  (^. responseBody) <$> (asJSON =<< getWith ops (unpack url) :: IO (Response a))
