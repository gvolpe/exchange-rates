{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Config where

import           Data.Monoid                    ( (<>) )
import           Data.Text                      ( Text
                                                , unpack
                                                )
import           Dhall
import           GHC.Generics

type ApiKey = Text

data ForexConfig = ForexConfig
  { apiHost :: Text
  , apiKey :: ApiKey
  , apiPath :: Text
  , apiUsage :: Text
  , apiKeyExpiration :: Natural
  , apiReqPerHour :: Natural
  } deriving Generic

data RedisConfig = RedisConfig
  { redisHost :: Text
  , redisPort :: Natural
  } deriving (Generic, Show)

data AppConfig = AppConfig
  { forex :: ForexConfig
  , redis :: RedisConfig
  } deriving (Generic, Show)

instance Show ForexConfig where
  show c =
    "ForexConfig {host = \""
      ++ unpack (apiHost c <> apiPath c)
      ++ "\", apiKey = [SECRET], keyExpiration = "
      ++ show (apiKeyExpiration c)
      ++ "}"

instance Interpret ForexConfig
instance Interpret RedisConfig
instance Interpret AppConfig

loadConfig :: IO AppConfig
loadConfig = input auto "./config/app.dhall"
