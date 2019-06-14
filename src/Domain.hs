module Domain where

newtype Exchange = Exchange { value :: Float } deriving Show

data Currency = USD | EUR | GBP | PLN | ARS deriving Show
