module Utils where

-- Alias for `tap`
(>>>) :: Monad m => m a -> (a -> m b) -> m a
(>>>) = tap

tap :: Monad m => m a -> (a -> m b) -> m a
tap ma f = ma >>= (\a -> a <$ f a)

unit :: Applicative m => m ()
unit = pure ()

maybeToEither :: b -> Maybe a -> Either b a
maybeToEither _ (Just a) = Right a
maybeToEither b Nothing  = Left b
