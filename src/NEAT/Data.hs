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
  deriving (Eq, Ord, Show)

data EnableStatus = Enabled | Disabled
  deriving (Show)

data NodeKind = Sensor | Hidden | Output
  deriving (Eq, Show)

-- TODO @incomplete: differentiate between input and hidden node?
data Node = Node
  { nodeId :: NodeId
  , kind :: NodeKind}
  deriving (Eq, Show)

data Edge = Edge
  -- TODO @incomplete: add the bias
  -- TODO @incomplete: do we need to constraint the range the weight and the bias
  { inNodeId     :: NodeId
  , outNodeId    :: NodeId
  , weight       :: Float
  , enableStatus :: EnableStatus
  , gin          :: GIN}
  deriving (Show)

data Genome = Genome
  { nodes :: Map.Map NodeId Node
  , edges :: Vector Edge}
  deriving (Show)

data Mismatch = Disjoint | Excess

data Trither a b
  = Both a b
  | Sinistra Mismatch a
  | Destra Mismatch b


type Population = Vector Genome

data Config = Config
  { name :: String
  , initPopulation :: Int
  , weightRange :: (Float, Float)
  , guessedGenerations :: Int
  , threshold :: Float}
