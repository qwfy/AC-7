{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

module NEAT.Data where

import qualified Data.UUID
import Data.Vector (Vector)
import qualified Data.Map.Strict as Map

-- | Global Innovation Number.
newtype GIN = GIN Integer
  deriving (Show)

newtype NodeId = NodeId Data.UUID.UUID
  deriving (Eq, Ord)

data EnableStatus = Enabled | Disabled

-- TODO @incomplete: differentiate between input and hidden node?
data Node = Node
  { nodeId :: NodeId
  }

data Edge = Edge
  -- TODO @incomplete: add the bias
  -- TODO @incomplete: do we need to constraint the range the weight and the bias
  { inNodeId     :: NodeId
  , outNodeId    :: NodeId
  , weight       :: Float
  , enableStatus :: EnableStatus
  , gin          :: GIN
  }

data Genome = Genome
  { nodes :: Map.Map NodeId Node
  , edges :: Vector Edge
  }

data Mismatch = Disjoint | Excess

data Trither a b
  = Both a b
  | Sinistra Mismatch a
  | Destra Mismatch b
