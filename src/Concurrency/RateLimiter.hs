module Concurrency.RateLimiter
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
import           Refined
import           Time
import           Transient.Base
import           Transient.Indeterminism

type RateLimit = Refined Positive Int

{- Evaluate a computation every certain amount of time while not exceeding a given rate limit
   and perform an action on every obtained `a`.

   eg. given:
     1. an `IO a` that represents a remote API call
     2. a rate limit of 100 calls per hour

   This function will make a call every 36 seconds asynchronously.
-}
rateLimiter :: (Typeable a) => Duration -> RateLimit -> IO a -> (a -> TransIO ()) -> TransIO ()
rateLimiter duration rate fa action = f
 where
  n = toMicroSeconds duration `div` unrefine rate
  f = async fa >>= action >> liftIO (threadDelay n) >> f :: TransIO ()

-- Sequence a list of computations in parallel just for the effects
parSequenceN :: (Typeable a) => Int -> [IO a] -> IO ()
parSequenceN n xs = void . keep' $ threads n (choose xs) >>= liftIO

-- Traverse a list in parallel with a limit `n`
parTraverseN :: (Typeable b) => Int -> (a -> IO b) -> [a] -> IO [b]
parTraverseN n f xs = do
  ref <- liftIO $ newIORef [] :: IO (IORef [b])
  keep' $ do
    a <- threads n (choose xs)
    b <- liftIO $ f a
    liftIO $ atomicModifyIORef' ref (\x -> (x, x ++ [b]))
  readIORef ref
