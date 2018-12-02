module Data.AC7 where


newtype RunId = RunId String

instance Show RunId where
  show (RunId x) = "RunId " ++ x
