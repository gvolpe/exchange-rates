module Cache.RedisClient
  ( redisConnect
  )
where

import           Config
import           Data.Text                      ( unpack )
import           Database.Redis
import           GHC.Natural                    ( naturalToInteger )

connInfo c = defaultConnectInfo
  { connectHost = unpack $ redisHost c
  , connectPort = PortNumber (fromInteger . naturalToInteger $ redisPort c)
  }

redisConnect :: RedisConfig -> IO Connection
redisConnect = checkedConnect . connInfo
