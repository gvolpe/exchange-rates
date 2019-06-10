module Time where

data TimeUnit = Seconds | Minutes | Hours deriving (Show)

data Duration = Duration
  { unit :: Int
  , timeUnit :: TimeUnit
  } deriving (Show)

seconds :: Int -> Duration
seconds n = Duration n Seconds

minutes :: Int -> Duration
minutes n = Duration n Minutes

hours :: Int -> Duration
hours n = Duration n Hours

toNanoSeconds :: Duration -> Int
toNanoSeconds (Duration n Seconds) = n * 1000000
toNanoSeconds (Duration n Minutes) = n * 60 * 1000000
toNanoSeconds (Duration n Hours  ) = n * 60 * 60 * 1000000
