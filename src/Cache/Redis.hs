{-# LANGUAGE BlockArguments, LambdaCase #-}

module Cache.Redis
  ( Cache(..)
  , mkRedisCache
  )
where

import           Config
import           Control.Monad.IO.Class         ( liftIO )
import qualified Data.ByteString.Char8         as BS
import           Data.Functor                   ( (<&>)
                                                , void
                                                )
import           Data.Text                      ( unpack )
import           Database.Redis
import           Domain.Currency                ( Currency )
import           Domain.Model                   ( Exchange(..)
                                                , Expiration(..)
                                                )
import           GHC.Natural                    ( naturalToInteger )

data Cache m = Cache
  { cacheNewResult :: Expiration -> Currency -> Currency -> Exchange -> m ()
  , cachedExchange :: Currency -> Currency -> m (Maybe Exchange)
  }

mkRedisCache :: RedisConfig -> IO (Cache IO)
mkRedisCache cfg =
  redisConnect cfg
    <&> (\c -> Cache { cacheNewResult = cacheNewResult' c
                     , cachedExchange = cachedExchange' c
                     }
        )

cacheNewResult'
  :: Connection -> Expiration -> Currency -> Currency -> Exchange -> IO ()
cacheNewResult' conn x from to ex = runRedis conn $ do
  hset k f v
  void $ expire k (getExpiration x)
 where
  k = BS.pack $ show from
  f = BS.pack $ show to
  v = BS.pack . show $ getExchange ex

cachedExchange' :: Connection -> Currency -> Currency -> IO (Maybe Exchange)
cachedExchange' conn from to =
  runRedis conn (hget (BS.pack $ show from) (BS.pack $ show to)) <&> \case
    Right (Just x) -> Just $ Exchange (read $ BS.unpack x :: Float)
    _              -> Nothing

-- Redis connection --
connInfo c = defaultConnectInfo
  { connectHost = unpack $ redisHost c
  , connectPort = PortNumber (fromInteger . naturalToInteger $ redisPort c)
  }

redisConnect :: RedisConfig -> IO Connection
redisConnect = checkedConnect . connInfo
