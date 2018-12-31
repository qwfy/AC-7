{-# LANGUAGE NamedFieldPuns #-}

module NEAT.Data where

import qualified Data.UUID
import Data.Vector (Vector)
import Data.Map (Map)
import Random


newtype NodeId = NodeId Data.UUID.UUID
  deriving (Eq, Ord, Show)

data NodeKind
  = Sensor Int -- ^ Sensors are aligned with integers during mating and input feeding, counts from 0
  | Hidden
  | Output Int -- ^ Outputs are aligned with integers during mating and output extraction, counts from 0
  deriving (Eq, Show)

data Node = Node
  { nodeId :: NodeId
  , kind :: NodeKind}
  deriving (Eq)

-- | Global Innovation Number.
newtype GIN = GIN Int deriving (Show)

data EnableStatus
  = Enabled
  | Disabled
  deriving (Eq, Show)

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

data MutateParams = MutateParams
  { mutateWeightP   :: P -- ^ Probability of mutating an edge's weight
  , perturbeWeightP :: P -- ^ Out of 'mutateWeightP', probability of perturbing the weight. If the weight is not perturbed, then it is replaced with a completely new one, sampled from 'weightRange'
  , perturbeRange   :: (Float, Float) -- ^ Uniformlly choose a random number from this range and add it the the current weight, if so desired

  , mutateAddNodeP  :: P -- ^ Probability of adding a new node
  , mutateAddEdgeP  :: P -- ^ Probability of adding a new edge
  , geneDisableP    :: P -- ^ A gene is disabled with this probability if it is disabled in either parent
  }

data Params = Params
  { name :: String
  , numInitPopulation :: Int
  , weightRange :: (Float, Float)
  , numGuessedGenerations :: Int
  , compatibilityParams :: CompatibilityParams
  , numInNodes :: Int
  , numOutNodes :: Int
  , mutateParams :: MutateParams}

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
