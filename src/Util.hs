module Util
  ( (|>)
  , currentTime
  )
where

import Data.Time

infixl 0 |>
(|>) :: a -> (a -> b) -> b
a |> f = f a


currentTime :: IO String
currentTime =
  formatTime defaultTimeLocale "%Y%m%d_%H%M%S_%z" <$> getCurrentTime
