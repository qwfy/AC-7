module Util
  ( (|>)
  )
where

infixl 0 |>
a |> f = f a
