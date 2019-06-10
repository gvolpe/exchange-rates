module RateLimiter
  ( RateLimit
  , parSequenceN
  , parTraverseN
  , rateLimiter
  )
where

import           Control.Concurrent             ( threadDelay )
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Functor                   ( void )
import           Data.IORef
import           Data.Typeable                  ( Typeable )
import           Time
import           Transient.Base
import           Transient.Indeterminism

type RateLimit = Int

-- Evaluate a computation every certain amount of time while not exceeding a given rate limit.
--
-- eg. given:
--   1. an `IO a` that represents a remote API call
--   2. a rate limit of 100 calls per hour
--
-- this function will make a call every 36 seconds asynchronously
rateLimiter :: (Typeable a) => Duration -> RateLimit -> IO a -> IO ()
rateLimiter duration rate fa = void . keep' $ f
 where
  n = toNanoSeconds duration `div` rate
  f = async (fa >> threadDelay n) >> f :: TransIO ()

-- Sequence a list of computations in parallel just for the effects
parSequenceN :: (Typeable a) => Int -> [IO a] -> IO ()
parSequenceN n xs = void . keep' $ threads n (choose xs) >>= liftIO

-- Traverse a list in parallel with a limit `n`
parTraverseN :: (Typeable a) => Int -> (a -> IO b) -> [a] -> IO [b]
parTraverseN n f xs = do
  ref <- liftIO $ newIORef [] :: IO (IORef [b])
  keep' $ do
    a <- threads n (choose xs)
    b <- liftIO $ f a
    liftIO $ modifyIORef' ref (\x -> x ++ [b])
  readIORef ref
