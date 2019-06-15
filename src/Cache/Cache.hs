{-# LANGUAGE BlockArguments, LambdaCase #-}

module Cache.Cache
  ( RedisCache(..)
  , mkRedisCache
  )
where

import           Cache.RedisClient              ( redisConnect )
import           Config
import           Control.Monad.IO.Class         ( liftIO )
import qualified Data.ByteString.Char8         as BS
import           Data.Functor                   ( (<&>)
                                                , void
                                                )
import           Data.Text                      ( unpack )
import           Database.Redis
import           Domain

data RedisCache = RedisCache
  { cacheNewResult :: Currency -> Currency -> Exchange -> IO ()
  , cachedExchange :: Currency -> Currency -> IO (Maybe Exchange)
  }

mkRedisCache :: RedisConfig -> IO RedisCache
mkRedisCache cfg =
  redisConnect cfg
    <&> (\c -> RedisCache { cacheNewResult = cacheNewResult' c
                          , cachedExchange = cachedExchange' c
                          }
        )

cacheNewResult' :: Connection -> Currency -> Currency -> Exchange -> IO ()
cacheNewResult' conn from to ex = runRedis conn $ do
  hset k f v
  void $ expire k (60 * 20) -- expire in 20 minutes
 where
  k = BS.pack $ show from
  f = BS.pack $ show to
  v = BS.pack . show $ value ex

cachedExchange' :: Connection -> Currency -> Currency -> IO (Maybe Exchange)
cachedExchange' conn from to =
  runRedis conn (hget (BS.pack $ show from) (BS.pack $ show to)) <&> \case
    Right (Just x) -> Just $ Exchange (read $ BS.unpack x :: Float)
    _              -> Nothing
