module Log
  ( info
  )
  where

info :: [String] -> IO ()
info = putStrLn . unwords
