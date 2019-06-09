module Main where

import           Config
import           Forex                          ( showApiUsage
                                                , showForex
                                                )

main :: IO ()
main = do
  c <- loadConfig
  print c
  showApiUsage (forex c)
