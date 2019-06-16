module Rates.UtilsTest where

import           Data.List.NonEmpty             ( head
                                                , last
                                                )
import           Hedgehog
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range
import           Prelude                 hiding ( head
                                                , last
                                                )
import           Utils                          ( (>>>) )

prop_tap_maybe :: Property
prop_tap_maybe = property $ do
  xs <- forAll $ Gen.nonEmpty (Range.linear 0 100) Gen.alpha
  (Just (head xs) >>> const (Just $ last xs)) === Just (head xs)
