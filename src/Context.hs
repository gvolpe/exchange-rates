module Context where

import           Config                         ( AppConfig(..)
                                                , ForexConfig
                                                , RedisConfig
                                                )
import           Data.Interface                 ( Cache
                                                , ForexClient
                                                )
import           Logger                         ( Logger )
import           RIO

data Ctx = Ctx
  { getLogger :: Logger IO
  , getCache :: Cache IO
  , getForexClient :: ForexClient IO
  }

newtype Env = Env { getAppConfig :: AppConfig }

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

class HasAppConfig env where
  appConfigL :: Lens' env AppConfig

instance HasAppConfig Env where
  appConfigL = lens getAppConfig (\x y -> x { getAppConfig = y })

class HasForexConfig env where
  forexConfigL :: Lens' env ForexConfig

instance HasForexConfig Env where
  forexConfigL = appConfigL . lens forex (\x y -> x { forex = y })

class HasRedisConfig env where
  redisConfigL :: Lens' env RedisConfig

instance HasRedisConfig Env where
  redisConfigL = appConfigL . lens redis (\x y -> x { redis = y })
