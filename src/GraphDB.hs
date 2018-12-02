module GraphDB
  ( loop
  , Poison(..)
  )
  where

import Control.Concurrent.STM
import Control.Monad
import qualified Database.Bolt as Bolt
import Data.Text (Text)


data Poison a
  = Poison
  | Payload a


loop :: Bolt.BoltCfg -> TBQueue (Poison Text) -> IO ()
loop boltCfg queryQueue = do
  pipe <- Bolt.connect boltCfg
  go pipe queryQueue
  where
    go pipe queue = do
      query' <- atomically $ readTBQueue queue
      case query' of
        Poison -> return ()
        Payload query -> do
          Bolt.run pipe $ Bolt.query_ query
          go pipe queue
