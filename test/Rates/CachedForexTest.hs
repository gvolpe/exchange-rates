{-# LANGUAGE TemplateHaskell #-}

module Rates.CachedForexTest
  ( cachedForexServiceTests
  )
where

import           Cache.Redis                    ( Cache(..) )
import           Context                        ( Ctx(..) )
import           Data.Functor                   ( (<&>)
                                                , void
                                                )
import           Data.IORef
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Domain.Currency
import           Domain.Model                   ( Expiration(..)
                                                , Exchange(..)
                                                )
import           Logger                         ( Logger(..) )
import           Hedgehog
import qualified Hedgehog.Gen                  as Gen
import           Http.Client.Forex              ( ForexClient(..) )
import           Prelude                 hiding ( head
                                                , last
                                                )
import           RIO                     hiding ( atomicModifyIORef
                                                , newIORef
                                                , readIORef
                                                , to
                                                )
import           Service.CachedForex            ( ExchangeService(..)
                                                , mkExchangeService
                                                )
import           Utils                          ( unit )

type TestCache = IORef (Map (Currency, Currency) Exchange)

testCacheNewResult
  :: TestCache -> Expiration -> Currency -> Currency -> Exchange -> IO ()
testCacheNewResult ref _ from to rate =
  void $ atomicModifyIORef ref (\m -> (m, Map.insert (from, to) rate))

testCachedExchange :: TestCache -> Currency -> Currency -> IO (Maybe Exchange)
testCachedExchange ref from to = do
  cached <- Map.lookup (from, to) <$> readIORef ref
  pure $ (const $ Exchange 2.0) <$> cached

mkTestCache :: IO (Cache IO)
mkTestCache =
  (newIORef Map.empty :: IO TestCache)
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

testLogger :: Logger IO
testLogger = Logger (const unit)

prop_get_rates :: Property
prop_get_rates = withTests 1000 $ property $ do
  cache <- evalIO mkTestCache
  let ctx = Ctx testLogger cache testForexClient
  service <- evalIO $ runRIO ctx mkExchangeService
  from    <- forAll $ Gen.element currencies
  to      <- forAll $ Gen.element currencies
  result  <- evalIO $ getRate service from to
  result === Exchange 1.0

cachedForexServiceTests :: Group
cachedForexServiceTests = $$(discover)
