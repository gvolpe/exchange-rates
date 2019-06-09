module RateLimiter where

import           Control.Concurrent             ( threadDelay )
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Functor                   ( void )
import           Transient.Base
import           Transient.Indeterminism

rateLimiter :: Int -> [IO ()] -> IO ()
rateLimiter n [] = pure ()
rateLimiter n xs = void . keep' $ threads n (choose xs) >>= liftIO
