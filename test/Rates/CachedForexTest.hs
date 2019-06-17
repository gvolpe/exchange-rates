{-# LANGUAGE TemplateHaskell #-}

module Rates.CachedForexTest
  ( cachedForexServiceTests
  )
where

import           Cache.Redis                    ( Cache(..) )
import           Data.Functor                   ( (<&>), void )
import           Data.IORef
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Domain.Currency
import           Domain.Model                   ( Expiration(..)
                                                , Exchange(..)
                                                )
import           Hedgehog
import qualified Hedgehog.Gen                  as Gen
import           Http.Client.Forex              ( ForexClient(..) )
import           Prelude                 hiding ( head
                                                , last
                                                )
import           Service.CachedForex            ( ExchangeService(..)
                                                , mkExchangeService
                                                )

type Ref = IORef (Map (Currency, Currency) Exchange)

testCacheNewResult
  :: Ref -> Expiration -> Currency -> Currency -> Exchange -> IO ()
testCacheNewResult ref _ from to rate =
  void $ atomicModifyIORef ref (\m -> (m, Map.insert (from, to) rate))

testCachedExchange :: Ref -> Currency -> Currency -> IO (Maybe Exchange)
testCachedExchange ref from to = readIORef ref <&> (Map.lookup (from, to))

mkTestCache :: IO (Cache IO)
mkTestCache =
  (newIORef Map.empty :: IO Ref)
    <&> (\ref -> Cache { cacheNewResult = testCacheNewResult ref
                       , cachedExchange = testCachedExchange ref
                       }
        )

testCallForex :: Currency -> Currency -> IO Exchange
testCallForex _ _ = pure $ Exchange 1.0

testForexClient :: ForexClient IO
testForexClient = ForexClient { callForex   = testCallForex
                              , getApiUsage = undefined
                              , expiration  = undefined
                              }

prop_get_rates :: Property
prop_get_rates = property $ do
  from    <- forAll $ Gen.element currencies
  to      <- forAll $ Gen.element currencies
  cache   <- evalIO mkTestCache
  service <- evalIO $ mkExchangeService cache testForexClient
  result  <- evalIO $ getRate service from to
  result === Exchange 1.0

cachedForexServiceTests :: Group
cachedForexServiceTests = $$(discover)
