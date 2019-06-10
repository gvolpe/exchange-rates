module RateLimiter
  ( parSequenceN
  , parTraverseN
  )
where

import           Control.Concurrent             ( threadDelay )
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Functor                   ( void )
import           Data.IORef
import           Data.Typeable                  ( Typeable )
import           Transient.Base
import           Transient.Indeterminism

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

