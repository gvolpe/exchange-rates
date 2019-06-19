{-# LANGUAGE TemplateHaskell #-}

module Rates.UtilsTest
  ( tapTests
  )
where

import           Data.List.NonEmpty             ( head
                                                , last
                                                )
import           Hedgehog
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range
import           Prelude                 hiding ( head
                                                , last
                                                )
import           Utils                          ( tap
                                                , unit
                                                )

prop_tap_maybe :: Property
prop_tap_maybe = property $ do
  xs <- forAll $ Gen.nonEmpty (Range.linear 0 100) Gen.alpha
  (Just (head xs) `tap` const (Just $ last xs)) === Just (head xs)

prop_tap_io :: Property
prop_tap_io = property $ do
  x <- forAll Gen.alpha
  y <- evalIO $ pure x `tap` const unit
  x === y

tapTests :: Group
tapTests = $$(discover)
