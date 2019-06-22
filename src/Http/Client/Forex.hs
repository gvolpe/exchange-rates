{-# LANGUAGE OverloadedStrings, RankNTypes, ScopedTypeVariables #-}

module Http.Client.Forex
  ( ForexClient(..)
  , mkForexClient
  )
where

import           Config
import           Context
import           Control.Lens            hiding ( (^.)
                                                , view
                                                )
import           Data.Aeson              hiding ( Options )
import           Data.Aeson.Types        hiding ( Options )
import           Data.Interface                 ( ForexClient(..) )
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.Monoid                    ( (<>) )
import           Data.Text
import           Domain.Currency                ( Currency )
import           Domain.Model                   ( ApiUsage
                                                , Exchange(..)
                                                , Expiration(..)
                                                )
import           GHC.Generics                   ( Generic )
import           GHC.Natural                    ( naturalToInt
                                                , naturalToInteger
                                                )
import           Network.Wreq
import           RIO

instance FromJSON Exchange where
  parseJSON v = do
    j <- parseJSON v :: Parser (Map Text Value)
    case M.toList j of
      [(_, x)] -> Exchange <$> (parseJSON x :: Parser Float)

instance FromJSON ApiUsage

mkForexClient :: HasForexConfig ctx => RIO ctx (ForexClient IO)
mkForexClient =
  view forexConfigL
    <&> (\cfg -> ForexClient
          { callForex   = callForex' cfg
          , getApiUsage = getApiUsage' cfg
          , expiration  = Expiration (naturalToInteger $ keyExpiration cfg)
          , reqPerHour  = naturalToInt $ requestsPerHour cfg
          }
        )

callForex' :: ForexConfig -> Currency -> Currency -> IO Exchange
callForex' c from to =
  let url = apiHost c <> apiPath c <> "/convert"
      exc = param "q" .~ [pack $ show from <> "_" <> show to]
      key = param "apiKey" .~ [apiKey c]
      ops = defaults & exc & param "compact" .~ ["ultra"] & key
  in  req ops url

getApiUsage' :: ForexConfig -> IO ApiUsage
getApiUsage' c =
  let url = apiHost c <> apiUsage c
      ops = defaults & param "apiKey" .~ [apiKey c]
  in  req ops url

req :: forall a . FromJSON a => Options -> Text -> IO a
req ops url =
  (^. responseBody) <$> (asJSON =<< getWith ops (unpack url) :: IO (Response a))
