{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

{-
 - An alternative test using a stack of monad transformers instead of IO + IORef
 - just for demonstration purposes.
 -
 - In this case it's clearly easier to test in IO since the transformers stack is
 - quite cumbersome to work with.
 -
 - Note that we cannot test with Identity because we need an instance of MonadMask.
 -
 - The full signature of the final effect is the following:
 -
 - type RatesMap = Map (Currency, Currency) Exchange
 - type Error = SomeException
 -
 - ReaderT (Ctx (StateT RatesMap (ExceptT Error (Either Error)))) (StateT RatesMap (ExceptT Error (Either Error))) a
 -
 - Too long to write in one go so better to define some type aliases.
 -}
module Rates.CachedForexRST
  ( cachedForexServiceRST
  )
where

import           Context                        ( Ctx(..) )
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Domain.Currency
import           Domain.Model                   ( Expiration(..)
                                                , Exchange(..)
                                                )
import           Data.Interface                 ( Cache(..)
                                                , Counter(..)
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

type RatesMap = Map (Currency, Currency) Exchange
type Error = SomeException
type Eff = StateT RatesMap (ExceptT Error (Either Error))

testCacheNewResult :: Expiration -> Currency -> Currency -> Exchange -> Eff ()
testCacheNewResult _ from to rate =
  get >>= \m -> put (Map.insert (from, to) rate m)

testCachedExchange :: Currency -> Currency -> Eff (Maybe Exchange)
testCachedExchange from to = do
  cached <- get <&> Map.lookup (from, to)
  pure (Exchange 2.0 <$ cached)

testCache :: Cache Eff
testCache = Cache { cacheNewResult = testCacheNewResult
                  , cachedExchange = testCachedExchange
                  }

testCallForex :: Currency -> Currency -> Eff Exchange
testCallForex _ _ = pure $ Exchange 1.0

testForexClient :: ForexClient Eff
testForexClient = ForexClient { callForex   = testCallForex
                              , getApiUsage = undefined
                              , expiration  = undefined
                              , reqPerHour  = 100
                              }

testLogger :: Logger Eff
testLogger = Logger (const unit)

testCounter :: Counter Eff
testCounter = Counter { incrCount  = unit
                      , getCount   = pure 1
                      , resetCount = unit
                      }

ctx :: Ctx Eff
ctx = Ctx testLogger testCache testCounter testForexClient

program :: Currency -> Currency -> Eff (Maybe Exchange, Exchange)
program from to = do
  service <- runReaderT mkExchangeService ctx -- ReaderT (Ctx Eff) Eff (ExchangeService Eff)
  (,) <$> cachedExchange testCache from to <*> getRate service from to

prop_get_rates_st :: Property
prop_get_rates_st = withTests 1000 $ property $ do
  from <- forAll $ Gen.element currencies
  to   <- forAll $ Gen.element currencies
  let result = runExceptT (runStateT (program from to) Map.empty)
  Right ((_, rs), _) <- pure . join $ result
  rs === Exchange 1.0

prop_get_cached_rates_st :: Property
prop_get_cached_rates_st = withTests 1000 $ property $ do
  from <- forAll $ Gen.element currencies
  to   <- forAll $ Gen.element currencies
  let st     = Map.fromList [((from, to), Exchange 2.0)]
  let result = runExceptT (runStateT (program from to) st)
  Right ((cached, _), _) <- pure . join $ result
  cached === Just (Exchange 2.0)

cachedForexServiceRST :: Group
cachedForexServiceRST = $$(discover)
