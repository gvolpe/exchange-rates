module Context where

import           Cache.Redis                    ( Cache )
import           Http.Client.Forex              ( ForexClient )
import           Logger                         ( Logger )
import           RIO

data Ctx = Ctx
  { getLogger :: Logger IO
  , getCache :: Cache IO
  , getForexClient :: ForexClient IO
  }

class HasLogger ctx where
  loggerL :: Lens' ctx (Logger IO)

instance HasLogger Ctx where
  loggerL = lens getLogger (\x y -> x { getLogger = y })

class HasCache ctx where
  cacheL :: Lens' ctx (Cache IO)

instance HasCache Ctx where
  cacheL = lens getCache (\x y -> x { getCache = y })

class HasForexClient ctx where
  forexClientL :: Lens' ctx (ForexClient IO)

instance HasForexClient Ctx where
  forexClientL = lens getForexClient (\x y -> x { getForexClient = y })
