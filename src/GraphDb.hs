{-# LANGUAGE OverloadedStrings #-}
module GraphDb
  ( start
  , config
  , Poison(..)
  )
  where

import Control.Concurrent
import Control.Concurrent.STM
import qualified Database.Bolt as Bolt
import Data.Text (Text)
import Data.Default


data Poison a
  = Poison
  | Payload a

-- TODO @incomplete: read this from a config file
config :: Bolt.BoltCfg
config = def
  { Bolt.host = "127.0.0.1"
  , Bolt.user = "neo4j"
  , Bolt.password = "bigblueyellowsky7789"}

start :: Bolt.BoltCfg -> TBQueue (Poison Text) -> IO ThreadId
start boltCfg queryQueue = do
  pipe <- Bolt.connect boltCfg
  forkIO $ go pipe queryQueue
  where
    go :: Bolt.Pipe -> TBQueue (Poison Text) -> IO ()
    go pipe queue = do
      query' <- atomically $ readTBQueue queue
      case query' of
        Poison -> return ()
        Payload query -> do
          Bolt.run pipe $ Bolt.query_ query
          go pipe queue
