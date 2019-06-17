module Logger where

newtype Logger m = Logger { logInfo :: String -> m () }

mkLogger :: IO (Logger IO)
mkLogger = pure $ Logger putStrLn
