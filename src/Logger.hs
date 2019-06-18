module Logger where

newtype Logger m = Logger { logInfo :: String -> m () }

defaultLogger :: Logger IO
defaultLogger = Logger putStrLn
