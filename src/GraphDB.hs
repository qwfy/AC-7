module GraphDB
  ( loop
  )
  where

import Control.Concurrent.STM
import Control.Monad
import qualified Database.Bolt as Bolt
import Data.Text (Text)

loop :: Bolt.BoltCfg -> TBQueue Text -> IO ()
loop boltCfg queryQueue = do
  pipe <- Bolt.connect boltCfg
  forever $ do
    query <- atomically $ readTBQueue queryQueue
    Bolt.run pipe $ Bolt.query_ query
