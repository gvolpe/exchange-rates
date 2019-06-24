{-# LANGUAGE DeriveGeneric #-}

module Domain.Model where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.Text                      ( Text )
import           Domain.Currency
import           GHC.Generics                   ( Generic )

data ApiUsage = ApiUsage
  { timestamp :: String
  , usage :: Int
  } deriving (Generic, Show)

newtype Exchange = Exchange { getExchange :: Float } deriving (Eq, Show)

-- Represents expiration of cached keys in seconds
newtype Expiration = Expiration { getExpiration :: Integer } deriving Show

instance FromJSON Exchange where
  parseJSON v = do
    j <- parseJSON v :: Parser (Map Text Value)
    case M.toList j of
      [(_, x)] -> Exchange <$> (parseJSON x :: Parser Float)

instance FromJSON ApiUsage
