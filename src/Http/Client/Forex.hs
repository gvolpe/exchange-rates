{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables    #-}

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
import           Data.Aeson                     ( FromJSON )
import           Data.Interface                 ( ForexClient(..) )
import           Data.Monoid                    ( (<>) )
import           Data.Text
import           Domain.Currency                ( Currency )
import           Domain.Model
import           GHC.Generics                   ( Generic )
import           GHC.Natural                    ( naturalToInt
                                                , naturalToInteger
                                                )
import           Network.Wreq
import           RIO

mkForexClient :: HasForexConfig ctx => RIO ctx (ForexClient IO)
mkForexClient =
  view forexConfigL
    <&> (\cfg -> ForexClient
          { callForex   = callForex' cfg
          , getApiUsage = getApiUsage' cfg
          , expiration  = Expiration (naturalToInteger $ apiKeyExpiration cfg)
          , reqPerHour  = naturalToInt $ apiReqPerHour cfg
          }
        )

callForex' :: ForexConfig -> Currency -> Currency -> IO Exchange
callForex' ForexConfig {..} from to =
  let url = apiHost <> apiPath <> "/convert"
      exc = param "q" .~ [pack $ show from <> "_" <> show to]
      key = param "apiKey" .~ [apiKey]
      ops = defaults & exc & param "compact" .~ ["ultra"] & key
  in  req ops url

getApiUsage' :: ForexConfig -> IO ApiUsage
getApiUsage' ForexConfig {..} =
  let url = apiHost <> apiUsage
      ops = defaults & param "apiKey" .~ [apiKey]
  in  req ops url

req :: forall a . FromJSON a => Options -> Text -> IO a
req ops url =
  (^. responseBody) <$> (asJSON =<< getWith ops (unpack url) :: IO (Response a))
