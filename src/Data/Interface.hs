module Data.Interface where

import Domain.Currency
import Domain.Model

data Cache m = Cache
  { cacheNewResult :: Expiration -> Currency -> Currency -> Exchange -> m ()
  , cachedExchange :: Currency -> Currency -> m (Maybe Exchange)
  }

data Counter m = Counter
  { incrCount :: m ()
  , getCount :: m Int
  , resetCount :: m ()
  }

data ForexClient m = ForexClient
  { callForex :: Currency -> Currency -> m Exchange
  , getApiUsage :: m ApiUsage
  , expiration :: Expiration
  , reqPerHour :: Int
  }

newtype ExchangeService m = ExchangeService
  { getRate :: Currency -> Currency -> m Exchange
  }
