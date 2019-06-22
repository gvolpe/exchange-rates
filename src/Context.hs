{-# LANGUAGE FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses #-}

module Context where

import           Config                         ( AppConfig(..)
                                                , ForexConfig
                                                , RedisConfig
                                                )
import           Data.Interface                 ( Cache
                                                , Counter
                                                , ForexClient
                                                )
import           Logger                         ( Logger )
import           RIO

data Ctx m = Ctx
  { getLogger :: Logger m
  , getCache :: Cache m
  , getCounter :: Counter m
  , getForexClient :: ForexClient m
  }

newtype Env = Env { getAppConfig :: AppConfig }

class HasLogger ctx m | ctx -> m where
  loggerL :: Lens' ctx (Logger m)

instance Monad m => HasLogger (Ctx m) m where
  loggerL = lens getLogger (\x y -> x { getLogger = y })

class HasCache ctx m | ctx -> m where
  cacheL :: Lens' ctx (Cache m)

instance Monad m => HasCache (Ctx m) m where
  cacheL = lens getCache (\x y -> x { getCache = y })

class HasCounter ctx m | ctx -> m where
  counterL :: Lens' ctx (Counter m)

instance Monad m => HasCounter (Ctx m) m where
  counterL = lens getCounter (\x y -> x { getCounter = y })

class HasForexClient ctx m | ctx -> m where
  forexClientL :: Lens' ctx (ForexClient m)

instance Monad m => HasForexClient (Ctx m) m where
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
