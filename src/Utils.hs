module Utils
  ( (>>>)
  , tap
  )
where

-- Alias for `tap`
(>>>) :: Monad m => m a -> (a -> m b) -> m a
(>>>) = tap

tap :: Monad m => m a -> (a -> m b) -> m a
tap ma f = ma >>= (\a -> a <$ f a )
