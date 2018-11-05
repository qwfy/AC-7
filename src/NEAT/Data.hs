{-# LANGUAGE NamedFieldPuns #-}

module NEAT.Data
  ( makeGINTVar
  )
where

import qualified Data.List
import qualified Data.UUID
import qualified Data.UUID.V4
import qualified Data.Vector as Vector
import Data.Vector (Vector)
import qualified System.Random
import qualified Random
import qualified Data.Map.Strict as Map
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM (STM, TVar)


-- | Global Innovation Number.
newtype GIN = GIN Integer
  deriving (Show)

makeGINTVar :: GIN -> IO (TVar GIN)
makeGINTVar initialValue =
  STM.newTVarIO initialValue

increaseGIN :: TVar GIN -> STM GIN
increaseGIN var = do
  GIN oldV <- STM.readTVar var
  let new = GIN (oldV + 1)
  STM.writeTVar var new
  return new

newtype NodeId = NodeId Data.UUID.UUID
  deriving (Eq, Ord)

data EnableStatus = Enabled | Disabled

-- TODO @incomplete: differentiate between input and hidden node?
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
  , edges :: Vector Edge
  }

mutateExistingEdge :: Edge -> Random.P -> (Float, Float) -> IO Edge
mutateExistingEdge old weightMutateP weightRandomRange = do
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

-- TODO @incomplete: reuse GIN if the same mutation already occurred in the current generation
mutateAddNode :: Genome -> TVar GIN -> IO Genome
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
      let edge = edges Vector.! index
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

      -- NB: the order is significant
      let newEdges = Vector.update edges (Vector.singleton (index, disabledEdge))
      let newEdges = newEdges Vector.++ Vector.fromList [edgeToNew, edgeFromNew]

      return $ Genome {nodes = newNodes, edges = newEdges}

-- TODO @incomplete: reuse GIN if the same mutation already occurred in the current generation
mutateAddEdge :: Genome -> TVar GIN -> (Float, Float) -> IO Genome
mutateAddEdge old@(Genome {nodes, edges}) ginVar weightRange =
  -- according to the section 3.1 of the paper:
  --
  -- In the add connection mutation, a single new connection gene with
  -- a random weight is added connecting two previously unconnected nodes.
  let nodeIds = Map.keys nodes
      allPairs = [(f, t) | f <- nodeIds, t <- nodeIds]
      connectedPairs = Vector.toList $ Vector.map (\edge -> (inNodeId edge, outNodeId edge)) edges
      -- TODO @incomplete: can this be more efficient?
      unconnectedPairs = allPairs Data.List.\\ connectedPairs
  in if null unconnectedPairs
       then return old
       else do
         let maxIndex = length unconnectedPairs - 1
         index <- System.Random.randomRIO (0, maxIndex)
         newGIN <- STM.atomically $ increaseGIN ginVar
         newWeight <- System.Random.randomRIO weightRange
         let (fromNodeId, toNodeId) = unconnectedPairs !! index
         let newEdge = Edge
               { inNodeId = fromNodeId
               , outNodeId = toNodeId
               , weight = newWeight
               , enableStatus = Enabled
               , gin = newGIN
               }
         let newEdges = Vector.snoc edges newEdge
         return $ old {edges = newEdges}
data Trither a b
  = Both a b
  | Left' a
  | Right' b

increaseIndex _ Nothing = Nothing
increaseIndex maxIndex (Just i) =
  if i < maxIndex
    then Just (i + 1)
    else Nothing

-- | NB: This function relies on the fact that the edges are monotonically increasing.
-- TODO @incomplete: should we remove this assumption?
zipEdges :: Vector Edge -> Vector Edge -> [Trither Edge Edge]
zipEdges lefts rights =
  case (Vector.null lefts, Vector.null rights) of
    (True, True) -> []
    (True, False) -> map (\edge -> Right' edge) (Vector.toList lefts)
    (False, True) -> map (\edge -> Left' edge) (Vector.toList rights)
    (False, False) ->
      let (_, _, reversed) = Data.List.foldl' (
            \acc@(lIndex, rIndex, accEdges) _ ->
              case (lIndex, rIndex) of
                (Nothing, Nothing) -> acc
                (ll@(Just li), rr@Nothing) -> (increaseL ll, rr, Left' (lefts Vector.! li) : accEdges)
                (ll@Nothing, rr@(Just ri)) -> (ll, increaseR rr, Right' (rights Vector.! ri) : accEdges)
                (ll@(Just li), rr@(Just ri)) ->
                  case compare li ri of
                    -- historical markings match
                    EQ -> (increaseL ll, increaseR rr, Both (lefts Vector.! li) (rights Vector.! ri) : accEdges)
                    -- 1 2 3 4
                    -- 1 2 8
                    LT -> (increaseL ll, rr, Left' (lefts Vector.! li) : accEdges)
                    -- 1 2 8
                    -- 1 2 3 4
                    GT -> (ll, increaseR rr, Right' (rights Vector.! ri) : accEdges)
            ) (Just 0, Just 0, []) [1 .. lLength + rLength]
      in reverse reversed
  where
    lLength = Vector.length lefts
    rLength = Vector.length rights
    increaseL = increaseIndex (lLength - 1)
    increaseR = increaseIndex (rLength - 1)
