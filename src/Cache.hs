{-# LANGUAGE BlockArguments, LambdaCase #-}

module Cache
  ( cachedExchange
  , cacheNewResult
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

connInfo c = defaultConnectInfo
  { connectHost = unpack $ redisHost c
  , connectPort = PortNumber (fromInteger . naturalToInteger $ redisPort c)
  }

cacheNewResult :: RedisConfig -> Currency -> Currency -> Exchange -> IO ()
cacheNewResult c from to ex = do
  conn <- checkedConnect $ connInfo c
  runRedis conn $ do
    hset k f v
    void $ expire k (60 * 20) -- expire in 20 minutes
 where
  k = BS.pack $ show from
  f = BS.pack $ show to
  v = BS.pack . show $ value ex

cachedExchange :: RedisConfig -> Currency -> Currency -> IO (Maybe Exchange)
cachedExchange c from to = do
  conn <- checkedConnect $ connInfo c
  runRedis conn (hget (BS.pack $ show from) (BS.pack $ show to)) <&> \case
    Right (Just x) -> Just $ Exchange (read $ BS.unpack x :: Float)
    _              -> Nothing
