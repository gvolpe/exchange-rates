module Time where

data TimeUnit = Seconds | Minutes | Hours deriving (Show)

data Duration = Duration
  { unit :: Int
  , timeunit :: TimeUnit
  } deriving (Show)

seconds :: Int -> Duration
seconds n = Duration n Seconds

minutes :: Int -> Duration
minutes n = Duration n Minutes

hours :: Int -> Duration
hours n = Duration n Hours

toMicroSeconds :: Duration -> Int
toMicroSeconds (Duration n Seconds) = n * 1000000
toMicroSeconds (Duration n Minutes) = n * 60 * 1000000
toMicroSeconds (Duration n Hours  ) = n * 60 * 60 * 1000000
