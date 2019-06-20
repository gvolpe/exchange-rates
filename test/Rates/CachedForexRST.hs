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

import           Cache.Redis                    ( Cache(..) )
import           Context                        ( Ctx(..) )
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Functor                   ( void )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromMaybe )
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

type RatesMap = Map (Currency, Currency) Exchange
type Error = SomeException
type Eff = StateT RatesMap (ExceptT Error (Either Error))

testCacheNewResult :: Expiration -> Currency -> Currency -> Exchange -> Eff ()
testCacheNewResult _ from to rate = void $ Map.insert (from, to) rate <$> get

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
                              }

testLogger :: Logger Eff
testLogger = Logger (const unit)

exService :: ReaderT (Ctx Eff) Eff (ExchangeService Eff)
exService = mkExchangeService

program
  :: Ctx Eff
  -> Cache Eff
  -> Currency
  -> Currency
  -> Eff (Maybe Exchange, Exchange)
program ctx cache from to = do
  service <- runReaderT exService ctx
  cached  <- cachedExchange cache from to
  rate    <- getRate service from to
  pure (cached, rate)

prop_get_rates_st :: Property
prop_get_rates_st = withTests 1000 $ property $ do
  let cache = testCache
  let ctx   = Ctx testLogger cache testForexClient
  from <- forAll $ Gen.element currencies
  to   <- forAll $ Gen.element currencies
  let prg = program ctx cache from to
  Right ((cached, rs), _) <- pure . join $ runExceptT (runStateT prg Map.empty)
  rs === fromMaybe (Exchange 1.0) cached

cachedForexServiceRST :: Group
cachedForexServiceRST = $$(discover)
