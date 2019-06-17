{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Main where

import           Data.Functor                   ( void )
import           Hedgehog
import           Rates.UtilsTest

tests :: IO Bool
tests = checkParallel tapTests

main :: IO ()
main = void tests
