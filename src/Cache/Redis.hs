{-# LANGUAGE BlockArguments, LambdaCase #-}

module Cache.Redis
  ( Cache(..)
  , Expiration(..)
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
import           Domain
import           GHC.Natural                    ( naturalToInteger )

-- Represents expiration of cached keys in seconds
newtype Expiration = Expiration { getExpiration :: Integer } deriving Show

data Cache = Cache
  { cacheNewResult :: Expiration -> Currency -> Currency -> Exchange -> IO ()
  , cachedExchange :: Currency -> Currency -> IO (Maybe Exchange)
  }

mkRedisCache :: RedisConfig -> IO Cache
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
