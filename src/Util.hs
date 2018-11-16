module Util
  ( (|>)
  , currentTime
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


cartesian :: [a] -> [b] -> [(a, b)]
cartesian xs ys = do
  x <- xs
  y <- ys
  return (x, y)
