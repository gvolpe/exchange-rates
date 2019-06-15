{-# LANGUAGE DeriveGeneric #-}

module Domain where

import           GHC.Generics                   ( Generic )

data ApiUsage = ApiUsage
  { timestamp :: String
  , usage :: Int
  } deriving (Generic, Show)

newtype Exchange = Exchange { value :: Float } deriving Show

data Currency = USD | EUR | GBP | AUD | CAD | PLN | ARS deriving Show
