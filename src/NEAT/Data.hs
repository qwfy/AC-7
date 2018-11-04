{-# LANGUAGE NamedFieldPuns #-}

module NEAT.Data
  ( Node(..)
  , Edge(..)
  )
where

import qualified Data.UUID
import qualified Data.UUID.V4
import qualified System.Random
import qualified Random


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

data Genome = Genome
  { nodes :: [Node]
  , edges :: [Edge]
  }

mutateEdge :: Edge -> Random.P -> (Float, Float) -> IO Edge
mutateEdge old weightMutateP weightRandomRange = do
  -- mutate weight, according to the section 3.1 of the paper:
  --
  -- Connection weights mutate as in any NE system, with each connection either perturbed
  -- or not at each generation.
  shouldMutateWeight <- Random.trigger weightMutateP
  if shouldMutateWeight
    then do
      weightDelta <- System.Random.randomRIO weightRandomRange
      let newWeight = weight old + weightDelta
          new = old {weight = newWeight}
      return new
    else
      return old
