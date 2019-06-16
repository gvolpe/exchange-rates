{-# LANGUAGE DeriveGeneric #-}

module Domain.Currency where

import           Data.List                      ( elemIndex )
import           Data.Text                      ( Text
                                                , toUpper
                                                , pack
                                                )
import           GHC.Generics                   ( Generic )

data Currency = USD | EUR | GBP | AUD | CAD | PLN | ARS deriving (Generic, Enum, Show)

currencies :: [Currency]
currencies = enumFrom USD

parseCurrency :: Text -> Maybe Currency
parseCurrency t =
  let s = pack . show <$> currencies
      i = elemIndex (toUpper t) s
  in  (currencies !!) <$> i
