module GraphDB
  ( start
  , Poison(..)
  )
  where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import qualified Database.Bolt as Bolt
import Data.Text (Text)


data Poison a
  = Poison
  | Payload a


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
