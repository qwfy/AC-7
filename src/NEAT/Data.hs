{-# LANGUAGE NamedFieldPuns #-}

module NEAT.Data
  ( makeGINTVar
  )
where

import qualified Data.UUID
import qualified Data.UUID.V4
import qualified System.Random
import qualified Random
import qualified Data.Map.Strict as Map
import qualified Control.Concurrent.STM as STM


-- | Global Innovation Number.
newtype GIN = GIN Integer
  deriving (Show)

makeGINTVar :: GIN -> IO (STM.TVar GIN)
makeGINTVar initialValue =
  STM.newTVarIO initialValue

increaseGIN :: STM.TVar GIN -> STM.STM GIN
increaseGIN var = do
  GIN oldV <- STM.readTVar var
  let new = GIN (oldV + 1)
  STM.writeTVar var new
  return new

newtype NodeId = NodeId Data.UUID.UUID
  deriving (Eq, Ord)

data EnableStatus = Enabled | Disabled

data Node = Node
  { nodeId :: NodeId
  }

makeNode :: IO Node
makeNode = do
  nodeId <- Data.UUID.V4.nextRandom
  return $ Node {nodeId = NodeId nodeId}

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
  -- TODO @incomplete: for the edges, maybe use a map and key it with integer?
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

mutateAddNode :: Genome -> STM.TVar GIN -> IO Genome
mutateAddNode old@(Genome {nodes, edges}) ginVar =
  -- according to the section 3.1 of the paper:
  --
  -- In the add node mutation, an existing connection is split and the new node placed
  -- where the old connection used to be.
  -- The old connection is disabled and two new connections are added to the genome.
  -- The new connection leading into the new node receives a weigth of 1,
  -- and the new connection leading out receives the same weight as the old connection.
  if null edges
    then return old
    else do
      index <- System.Random.randomRIO (0, length edges - 1)
      let edge = edges !! index
      -- TODO @incomplete: what if the edge is already disabled?
      let disabledEdge = edge {enableStatus = Disabled}
      newNode <- makeNode

      -- in -> out
      -- becomes
      -- in -1-> new -old weight-> out
      ginToNew <- STM.atomically $ increaseGIN ginVar
      let edgeToNew = Edge
            { inNodeId = inNodeId disabledEdge
            , outNodeId = nodeId newNode
            , weight = 1.0
            , enableStatus = Enabled
            , gin = ginToNew
            }

      ginFromNew <- STM.atomically $ increaseGIN ginVar
      let edgeFromNew = Edge
            { inNodeId = nodeId newNode
            , outNodeId = outNodeId disabledEdge
            , weight = weight disabledEdge
            , enableStatus = Enabled
            , gin = ginFromNew
            }

      let newNodes = Map.insert (nodeId newNode) newNode nodes

      -- about the take and drop, say:
      -- index = 2, edges = [a b .c. d e]
      -- should take (2 = index)
      -- should drop (3 = index + 1)
      --
      -- NB: the order is significant
      -- TODO @incomplete: a more efficient implementation
      let newEdges = (take index edges) ++ [disabledEdge] ++ (drop (index+1) edges) ++ [edgeToNew, edgeFromNew]

      return $ Genome {nodes = newNodes, edges = newEdges}
