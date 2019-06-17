{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Main where

import           Data.Functor                   ( void )
import           Hedgehog
import           Rates.CachedForexTest
import           Rates.UtilsTest

main :: IO ()
main = do
  void $ checkParallel tapTests
  void $ checkParallel cachedForexServiceTests
