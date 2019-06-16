{-# LANGUAGE DeriveGeneric #-}

module Domain.Model where

import           Domain.Currency
import           GHC.Generics                   ( Generic )

data ApiUsage = ApiUsage
  { timestamp :: String
  , usage :: Int
  } deriving (Generic, Show)

newtype Exchange = Exchange { getExchange :: Float } deriving Show
