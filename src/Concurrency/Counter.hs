module Concurrency.Counter where

import           Data.Functor                   ( (<&>)
                                                , void
                                                )
import           Data.Interface                 ( Counter(..) )
import           Data.IORef

mkCounter :: IO (Counter IO)
mkCounter =
  (newIORef 0 :: IO (IORef Int))
    <&> (\ref -> Counter
          { incrCount  = void $ atomicModifyIORef' ref (\acc -> (acc + 1, acc))
          , getCount   = readIORef ref
          , resetCount = atomicWriteIORef ref 0
          }
        )
