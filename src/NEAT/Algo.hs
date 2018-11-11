{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

module NEAT.Algo
  ( simulate
  , makeInitPopulation
  , makeGINTVar
  )
where

import qualified Data.Maybe
import Data.List
import qualified Data.UUID.V4
import qualified Data.Vector as Vector
import Data.Vector (Vector)
import qualified System.Random
import qualified Random
import qualified Data.Map.Strict as Map
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM (STM, TVar)

import Util

import NEAT.Data

-- simulate :: population (Genome -> Float) -> Int -> IO ()
simulate = undefined

makeGINTVar :: GIN -> IO (TVar GIN)
makeGINTVar initialValue =
  STM.newTVarIO initialValue

increaseGIN :: TVar GIN -> STM GIN
increaseGIN var = do
  GIN oldV <- STM.readTVar var
  let new = GIN (oldV + 1)
  STM.writeTVar var new
  return new

makeNode :: IO Node
makeNode = do
  nodeId <- Data.UUID.V4.nextRandom
  return $ Node {nodeId = NodeId nodeId}

-- | Make a genome for the initial population, according to the section 3.4 of the paper:
--
-- In contrast, NEAT biases the search towards minimal-dimensional spaces
-- by starting out with a uniform population of networks with zero hidden nodes
-- (i.e., all inputs connect directly to putputs).
makeInitGenome :: (Float, Float) -> TVar GIN -> IO Genome
makeInitGenome weightRange ginVar = do
  inNode <- makeNode
  outNode <- makeNode
  weight <- System.Random.randomRIO weightRange
  gin <- STM.atomically $ increaseGIN ginVar
  let edge = Edge
        { inNodeId = nodeId inNode
        , outNodeId = nodeId outNode
        , weight
        , enableStatus = Enabled
        , gin}
  let genome = Genome
        { nodes = Map.fromList [(nodeId inNode, inNode), (nodeId outNode, outNode)]
        , edges = Vector.singleton edge}
  return genome

-- | Mutate weight, according to the section 3.1 of the paper:
--
-- Connection weights mutate as in any NE system, with each connection either perturbed
-- or not at each generation.
mutateExistingEdge :: Edge -> Random.P -> (Float, Float) -> IO Edge
mutateExistingEdge old weightMutateP weightRandomRange = do
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
            , gin = ginToNew}

      ginFromNew <- STM.atomically $ increaseGIN ginVar
      let edgeFromNew = Edge
            { inNodeId = nodeId newNode
            , outNodeId = outNodeId disabledEdge
            , weight = weight disabledEdge
            , enableStatus = Enabled
            , gin = ginFromNew}

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
      unconnectedPairs = allPairs \\ connectedPairs
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
               , gin = newGIN}
         let newEdges = Vector.snoc edges newEdge
         return $ old {edges = newEdges}

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
    (True, False) -> map (Destra Excess) (Vector.toList lefts)
    (False, True) -> map (Sinistra Excess) (Vector.toList rights)
    (False, False) ->
      let (_, _, reversed) = foldl' (
            \acc@(lIndex, rIndex, accEdges) _ ->
              case (lIndex, rIndex) of
                (Nothing, Nothing) -> acc
                (ll@(Just li), rr@Nothing) -> (increaseL ll, rr, Sinistra Excess (lefts Vector.! li) : accEdges)
                (ll@Nothing, rr@(Just ri)) -> (ll, increaseR rr, Destra Excess (rights Vector.! ri) : accEdges)
                (ll@(Just li), rr@(Just ri)) ->
                  case compare li ri of
                    -- historical markings match
                    EQ -> (increaseL ll, increaseR rr, Both (lefts Vector.! li) (rights Vector.! ri) : accEdges)
                    -- 1 2 3 4
                    -- 1 2 8
                    LT -> (increaseL ll, rr, Sinistra Disjoint (lefts Vector.! li) : accEdges)
                    -- 1 2 8
                    -- 1 2 3 4
                    GT -> (ll, increaseR rr, Destra Disjoint (rights Vector.! ri) : accEdges)
            ) (Just 0, Just 0, []) [1 .. lLength + rLength]
      in reverse reversed
  where
    lLength = Vector.length lefts
    rLength = Vector.length rights
    increaseL = increaseIndex (lLength - 1)
    increaseR = increaseIndex (rLength - 1)


cross :: (Genome, Float) -> (Genome, Float) -> IO Genome
cross (left, leftFitness) (right, rightFitness) = do
  let alignedEdges = zipEdges (edges left) (edges right)
  -- this is a list of random bools used to break ties
  triggers <- Random.triggers (length alignedEdges) (Random.P 0.5)

  let winner = case compare leftFitness rightFitness of
        EQ -> Nothing
        LT -> Just (Right ())
        GT -> Just (Left ())

  let pickLeft edge =
        (edge, [nodes left Map.! inNodeId edge, nodes left Map.! outNodeId edge])

  let pickRight edge =
        (edge, [nodes right Map.! inNodeId edge, nodes right Map.! outNodeId edge])

  -- TODO @incomplete: confirm that the node is calculated correctly
  let (finalEdges, dupNodes) =
        zip triggers alignedEdges
          |> map (\case
              (isLeft, Both l r) ->
                Just $ if isLeft then pickLeft l else pickRight r
              (isLeft, Sinistra _ l) ->
                case winner of
                  Just (Left ()) -> Just $ pickLeft l
                  Just (Right ()) -> Nothing
                  -- in the case of a tie, each conflict position is picked independently
                  Nothing -> if isLeft then Just (pickLeft l) else Nothing
              (isLeft, Destra _ r) ->
                case winner of
                  Just (Left ()) -> Nothing
                  Just (Right ()) -> Just $ pickRight r
                  -- in the case of a tie, each conflict position is picked independently
                  Nothing -> if isLeft then Nothing else Just (pickRight r))
          |> filter Data.Maybe.isJust |> map Data.Maybe.fromJust
          |> unzip

  let finalNodes = foldr (\node acc -> Map.insert (nodeId node) node acc) Map.empty (concat dupNodes)

  let child = Genome
        { nodes = finalNodes
        , edges = Vector.fromList finalEdges
        }

  return child

-- | This is the equation (1) in the paper.
compatibility :: (Float, Float, Float) -> Genome -> Genome -> Float
compatibility (c1, c2, c3) Genome{edges=edgesA} Genome{edges=edgesB} =
  let n' = max (Vector.length edgesA) (Vector.length edgesB)
      -- TODO @incomplete: make this configurable
      n = fromIntegral $ if n' < 20 then 1 else n'
      (nExcesses, nDisjoints, diffs) = foldl' (\(accNE, accND, accDiffs) trither ->
        case trither of
          Both n1 n2 ->
            -- TODO @incomplete: the paper does not mention abs
            let delta = abs (weight n1 - weight n2)
            in (accNE, accND, delta:accDiffs)
          Sinistra Disjoint _ -> (accNE, accND+1, accDiffs)
          Destra Disjoint _   -> (accNE, accND+1, accDiffs)
          Sinistra Excess _   -> (accNE+1, accND, accDiffs)
          Destra Excess _     -> (accNE+1, accND, accDiffs)
        ) (0, 0, []) (zipEdges edgesA edgesB)
      meanDiff = sum diffs / fromIntegral (length diffs)
  in c1 * nExcesses / n + c2 * nDisjoints / n + c3 * meanDiff

-- | Reproduce a species.
reproduce :: Vector (Genome, Float) -> Int -> IO (Vector Genome)
reproduce _ 0 = return Vector.empty
reproduce genomeFitnesses numOffsprings =
  Vector.replicateM numOffsprings $ do
    p1Fit <- Random.choose genomeFitnesses
    p2Fit <- Random.choose genomeFitnesses
    cross p1Fit p2Fit
makeInitPopulation :: Config -> TVar GIN -> IO Population
makeInitPopulation Config{initPopulation, weightRange} ginVar =
  Vector.generateM initPopulation (\_ -> makeInitGenome weightRange ginVar)
