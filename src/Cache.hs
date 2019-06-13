{-# LANGUAGE OverloadedStrings #-}

module Cache
  ( redisTest
  )
where

import           Config
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Text                      ( unpack )
import           Database.Redis
import           GHC.Natural                    ( naturalToInteger )

redisTest :: RedisConfig -> IO ()
redisTest c = do
  let redisConnInfo = defaultConnectInfo
        { connectHost = unpack $ redisHost c
        , connectPort = PortNumber
                          (fromInteger . naturalToInteger $ redisPort c)
        }
  conn <- checkedConnect redisConnInfo
  runRedis conn $ do
    set "foo" "bar"
    bar <- get "foo"
    liftIO $ print bar
