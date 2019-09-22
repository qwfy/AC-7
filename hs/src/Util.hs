module Util
  ( (|>)
  , currentTime
  , iso8601DatetimeWithTimezone
  , cartesian
  )
where

import Data.Time

infixl 0 |>
(|>) :: a -> (a -> b) -> b
a |> f = f a


currentTime :: IO String
currentTime =
  formatTime defaultTimeLocale "%Y%m%d_%H%M%S_%z" <$> getCurrentTime

iso8601DatetimeWithTimezone :: IO String
iso8601DatetimeWithTimezone =
  formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S_%Ez" <$> getCurrentTime


cartesian :: [a] -> [b] -> [(a, b)]
cartesian xs ys = do
  x <- xs
  y <- ys
  return (x, y)
