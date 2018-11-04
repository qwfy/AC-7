{-# LANGUAGE NamedFieldPuns #-}

module NEAT.Data
  ( Node(..)
  , Edge(..)
  )
where

import qualified Data.UUID
import qualified Data.UUID.V4


-- | Global Innovation Number.
newtype GIN = GIN Integer

data EnableStatus = Enabled | Disabled

data Node = Node
  { nodeId :: Data.UUID.UUID
  }

makeNode :: IO Node
makeNode = do
  nodeId <- Data.UUID.V4.nextRandom
  return $ Node {nodeId}

data Edge = Edge
  -- TODO @incomplete: add the bias
  -- TODO @incomplete: do we need to constraint the range the weight and the bias
  { inNode       :: Node
  , outNode      :: Node
  , weight       :: Float
  , enableStatus :: EnableStatus
  , gin          :: GIN
  }
