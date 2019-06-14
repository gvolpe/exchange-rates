{-# LANGUAGE OverloadedStrings #-}

module Cache
  ( redisTest
  , cachedExchange
  , cacheNewResult
  )
where

import           Config
import           Control.Monad.IO.Class         ( liftIO )
import qualified Data.ByteString.Char8         as BS
import           Data.Functor                   ( void )
import           Data.Text                      ( unpack )
import           Database.Redis
import           Domain
import           GHC.Natural                    ( naturalToInteger )

connInfo c = defaultConnectInfo
  { connectHost = unpack $ redisHost c
  , connectPort = PortNumber (fromInteger . naturalToInteger $ redisPort c)
  }

-- TODO: Set expire time
cacheNewResult :: RedisConfig -> Currency -> Currency -> Exchange -> IO ()
cacheNewResult c from to ex = do
  conn <- checkedConnect $ connInfo c
  runRedis conn $ void $ hset (BS.pack $ show from)
                              (BS.pack $ show to)
                              (BS.pack . show $ value ex)

cachedExchange :: RedisConfig -> Currency -> Currency -> IO (Maybe Exchange)
cachedExchange c from to = do
  conn <- checkedConnect $ connInfo c
  rs   <- runRedis conn $ hget (BS.pack $ show from) (BS.pack $ show to)
  pure $ case rs of
    Right (Just x) -> Just $ Exchange (read $ BS.unpack x :: Float)
    _              -> Nothing

redisTest :: RedisConfig -> IO ()
redisTest c = do
  conn <- checkedConnect $ connInfo c
  runRedis conn $ do
    set "foo" "bar"
    bar <- get "foo"
    liftIO $ print bar
