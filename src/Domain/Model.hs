{-# LANGUAGE DeriveGeneric #-}

module Domain.Model where

import           Domain.Currency
import           GHC.Generics                   ( Generic )

data ApiUsage = ApiUsage
  { timestamp :: String
  , usage :: Int
  } deriving (Generic, Show)

newtype Exchange = Exchange { getExchange :: Float } deriving Show

-- Represents expiration of cached keys in seconds
newtype Expiration = Expiration { getExpiration :: Integer } deriving Show
