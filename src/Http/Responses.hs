{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

module Http.Responses where

import           Data.Aeson
import           Data.Typeable                  ( Typeable )
import           Domain.Currency                ( Currency )
import           GHC.Generics                   ( Generic )

data ExchangeResponse = ExchangeResponse
  { rate :: Float
  , from :: Currency
  , to :: Currency
  } deriving (Generic, Show, Typeable)

instance ToJSON Currency
instance ToJSON ExchangeResponse
