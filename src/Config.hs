{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Config where

import           Data.Monoid                    ( (<>) )
import           Data.Text                      ( Text
                                                , unpack
                                                )
import           Dhall
import           GHC.Generics

type ApiKey = Text

data ForexConfig = ForexConfig
  { host :: Text
  , apiKey :: ApiKey
  , apiPath :: Text
  , apiUsage :: Text
  } deriving (Generic)

newtype AppConfig = AppConfig
  { forex :: ForexConfig
  } deriving (Show, Generic)

instance Show ForexConfig where
  show c =
    "ForexConfig {host = \""
      ++ unpack (host c <> apiPath c)
      ++ "\", apiKey = [SECRET]}"

instance Interpret ForexConfig
instance Interpret AppConfig

loadConfig :: IO AppConfig
loadConfig = input auto "./config/app.dhall"

