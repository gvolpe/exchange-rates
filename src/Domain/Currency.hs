{-# LANGUAGE DeriveGeneric #-}

module Domain.Currency where

import           Data.List                      ( elemIndex )
import           Data.Text                      ( Text
                                                , toUpper
                                                , pack
                                                )
import           GHC.Generics                   ( Generic )

data Currency = AED | AFN | ALL | AMD | ANG | AOA | ARS | AUD | AWG | AZN
  | BAM | BBD | BDT | BGN | BHD | BIF | BMD | BND | BOB | BRL | BSD | BTC
  | BTN | BWP | BYN | BYR | BZD | CAD | CDF | CHF | CLF | CLP | CNY | COP
  | CRC | CUC | CUP | CVE | CZK | DJF | DKK | DOP | DZD | EGP | ERN | ETB
  | EUR | FJD | FKP | GBP | GEL | GGP | GHS | GIP | GMD | GNF | GTQ | GYD
  | HKD | HNL | HRK | HTG | HUF | IDR | ILS | IMP | INR | IQD | IRR | ISK
  | JEP | JMD | JOD | JPY | KES | KGS | KHR | KMF | KPW | KRW | KWD | KYD
  | KZT | LAK | LBP | LKR | LRD | LSL | LVL | LYD | MAD | MDL | MGA | MKD
  | MMK | MNT | MOP | MRO | MUR | MVR | MWK | MXN | MYR | MZN | NAD | NGN
  | NIO | NOK | NPR | NZD | OMR | PAB | PEN | PGK | PHP | PKR | PLN | PYG
  | QAR | RON | RSD | RUB | RWF | SAR | SBD | SCR | SDG | SEK | SGD | SHP
  | SLL | SOS | SRD | STD | SVC | SYP | SZL | THB | TJS | TMT | TND | TOP
  | TRY | TTD | TWD | TZS | UAH | UGX | USD | UYU | UZS | VEF | VND | VUV
  | WST | XAF | XAG | XCD | XDR | XOF | XPF | YER | ZAR | ZMK | ZMW | ZWL
  deriving (Generic, Enum, Show)

currencies :: [Currency]
currencies = enumFrom AED

parseCurrency :: Text -> Maybe Currency
parseCurrency t =
  let s = pack . show <$> currencies
      i = elemIndex (toUpper t) s
  in  (currencies !!) <$> i
