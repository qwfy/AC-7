{-# LANGUAGE NamedFieldPuns #-}

module NEAT.Data where

import qualified Data.UUID
import Data.Vector (Vector)
import Data.Map (Map)


newtype NodeId = NodeId Data.UUID.UUID
  deriving (Eq, Ord, Show)

data NodeKind = Sensor | Hidden | Output
  deriving (Eq)

data Node = Node
  { nodeId :: NodeId
  , kind :: NodeKind}
  deriving (Eq)

-- | Global Innovation Number.
newtype GIN = GIN Integer

data EnableStatus = Enabled | Disabled

data Edge = Edge
  -- TODO @incomplete: add the bias
  -- TODO @incomplete: do we need to constraint the range the weight and the bias
  { inNodeId     :: NodeId
  , outNodeId    :: NodeId
  , weight       :: Float
  , enableStatus :: EnableStatus
  , gin          :: GIN}

data Genome = Genome
  { nodes :: Map NodeId Node
  , edges :: Vector Edge}

data Mismatch = Disjoint | Excess

data Trither a b
  = Both a b
  | Sinistra Mismatch a
  | Destra Mismatch b

data CompatibilityParams = CompatibilityParams
  { c123 :: (Float, Float, Float)
  , threshold :: Float}

data Params = Params
  { name :: String
  , numInitPopulation :: Int
  , weightRange :: (Float, Float)
  , guessedGenerations :: Int
  , compatibilityParams :: CompatibilityParams
  , inNodes :: Int
  , outNodes :: Int}

newtype OriginalFitness = OriginalFitness Float

newtype AdjustedFitness = AdjustedFitness Float

instance Show OriginalFitness where
  show (OriginalFitness f) = "OF=" ++ show f

instance Show AdjustedFitness where
  show (AdjustedFitness f) = "AF=" ++ show f

newtype Species = Species (Vector Genome)
newtype SpeciesWithAdjustedFitness =
  SpeciesWithAdjustedFitness (Vector (Genome, AdjustedFitness))

-- | A Generation is an ORDERED list of species, where its index encodes its species id.
-- Note that a species can be empty, and empty species will NOT be elimated.
newtype Generation = Generation (Vector Species)

newtype Population = Population (Vector Genome)
