{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Main where

import           Data.Functor                   ( void )
import           Hedgehog
import           Rates.CachedForexRST
import           Rates.CachedForexTest
import           Rates.UtilsTest

main :: IO ()
main = do
  forexTests <- cachedForexServiceTests
  void $ checkParallel tapTests
  void $ checkParallel forexTests
  void $ checkParallel cachedForexServiceRST
