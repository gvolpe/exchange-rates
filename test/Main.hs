{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Main where

import           Data.Functor                   ( void )
import           Hedgehog
import           Rates.UtilsTest                ( prop_tap_maybe )

tests :: IO Bool
tests =
  checkParallel $ Group "Rates.UtilsTest" [("prop_tap_maybe", prop_tap_maybe)]

main :: IO ()
main = void tests
