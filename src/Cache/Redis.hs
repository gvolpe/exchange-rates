{-# LANGUAGE BlockArguments, LambdaCase #-}

module Cache.Redis
  ( mkRedisCache
  )
where

import           Config
import           Context
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Functor                   ( (<&>)
                                                , void
                                                )
import           Data.Interface                 ( Cache(..) )
import           Data.Text                      ( unpack )
import           Database.Redis
import           Domain.Currency                ( Currency )
import           Domain.Model                   ( Exchange(..)
                                                , Expiration(..)
                                                )
import           GHC.Natural                    ( naturalToInteger )
import           RIO

import qualified Data.ByteString.Char8         as C

mkRedisCache :: HasRedisConfig env => RIO env (Cache IO)
mkRedisCache = do
  cfg <- view redisConfigL
  con <- liftIO $ redisConnect cfg
  pure Cache { cacheNewResult = cacheNewResult' con
             , cachedExchange = cachedExchange' con
             }

cacheNewResult'
  :: Connection -> Expiration -> Currency -> Currency -> Exchange -> IO ()
cacheNewResult' conn x from to ex = runRedis conn $ do
  hset k f v
  void $ expire k (getExpiration x)
 where
  k = C.pack $ show from
  f = C.pack $ show to
  v = C.pack . show $ getExchange ex

cachedExchange' :: Connection -> Currency -> Currency -> IO (Maybe Exchange)
cachedExchange' conn from to =
  runRedis conn (hget (C.pack $ show from) (C.pack $ show to)) <&> \case
    Right (Just x) -> Just $ Exchange (read $ C.unpack x :: Float)
    _              -> Nothing

-- Redis connection --
connInfo c = defaultConnectInfo
  { connectHost = unpack $ redisHost c
  , connectPort = PortNumber (fromInteger . naturalToInteger $ redisPort c)
  }

redisConnect :: RedisConfig -> IO Connection
redisConnect = checkedConnect . connInfo
