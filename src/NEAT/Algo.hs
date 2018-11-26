{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

module NEAT.Algo
  ( simulate
  , makeInitPopulation
  , makeGINTVar
  , genomeValue
  )
where

import Data.Maybe
import Data.List
import qualified Data.UUID.V4
import qualified Data.Vector as Vector
import Data.Vector (Vector)
import qualified System.Random
import qualified Random
import qualified Data.Map.Strict as Map
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM (STM, TVar)
import Control.Monad
import Safe
import Path
import Path.IO

import Util

import NEAT.Data
import NEAT.Vis
import Vis
import Random


-- TODO @incomplete: Things to consider:
--
-- - Interspecies breeding
-- https://stats.stackexchange.com/questions/272901/do-different-species-create-offspring-neat
-- This depends mostly on whether you want it to or not. Stanley and Miikkulainen do leave this pretty open, but in the NEAT C++ code which accompanies the paper, there is a parameter named interspecies_mate_rate which is often very low (around %0.1). Also, there is the parameter for stolen_babies which is usually disabled, but would also introduce interspecies breeding.
-- So to answer your question, you can get away with no interspecies breeding, but the theory of adding it, is that it could allow allow the best of both species to merge.
-- (It also models what occasionally occurs in nature, and is an interesting consideration)


makeGINTVar :: GIN -> IO (TVar GIN)
makeGINTVar initialValue =
  STM.newTVarIO initialValue

increaseGIN :: TVar GIN -> STM GIN
increaseGIN var = do
  GIN oldV <- STM.readTVar var
  let new = GIN (oldV + 1)
  STM.writeTVar var new
  return new

makeNode :: NodeKind -> IO Node
makeNode kind = do
  nodeId <- Data.UUID.V4.nextRandom
  return $ Node {nodeId = NodeId nodeId, kind = kind}

-- | Make a genome for the initial population, according to the section 3.4 of the paper:
--
-- In contrast, NEAT biases the search towards minimal-dimensional spaces
-- by starting out with a uniform population of networks with zero hidden nodes
-- (i.e., all inputs connect directly to putputs).
-- TODO @incomplete: replace (float, float) with a function that returns a random value
-- TODO @incomplete: ability to evolving input and output node as needed - comparing to
-- the current implementation where the number of inputs and outputs are fixed before hand
makeInitGenome :: (Float, Float) -> Int -> Int -> TVar GIN -> IO Genome
makeInitGenome weightRange numInNodes numOutNodes ginVar = do
  inNodes <- replicateM numInNodes $ makeNode Sensor
  outNodes <- replicateM numOutNodes $ makeNode Output
  edges <- mapM (\(inNode, outNode) -> do
    weight <- System.Random.randomRIO weightRange
    gin <- STM.atomically $ increaseGIN ginVar
    let edge = Edge
          { inNodeId = nodeId inNode
          , outNodeId = nodeId outNode
          , weight
          , enableStatus = Enabled
          , gin}
    return edge
    ) (cartesian inNodes outNodes)
  let genome = Genome
        { nodes = Map.fromList $ map (\node -> (nodeId node, node)) (inNodes ++ outNodes)
        , edges = Vector.fromList edges}
  return genome

-- | Mutate weight, according to the section 3.1 of the paper:
--
-- Connection weights mutate as in any NE system, with each connection either perturbed
-- or not at each generation.
-- TODO @incomplete: remove the mutation desion out of this function
mutateExistingEdge :: P -> P -> (Float, Float) -> (Float, Float) -> Edge -> IO Edge
mutateExistingEdge mutateP perturbeP perturbeRange weightRange old = do
  shouldMutate <- Random.trigger mutateP
  if shouldMutate
    then do
      shouldPerturbe <- Random.trigger perturbeP
      newWeight <-
        if shouldPerturbe
          then do
            weightDelta <- System.Random.randomRIO perturbeRange
            let newWeight = weight old + weightDelta
            return newWeight
          else
            System.Random.randomRIO weightRange
      let new = old {weight = newWeight}
      return new
    else
      return old

-- TODO @incomplete: reuse GIN if the same mutation already occurred in the current generation
mutateAddNode :: Genome -> TVar GIN -> IO Genome
mutateAddNode old@Genome{nodes, edges} ginVar =
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
      newNode <- makeNode Hidden

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
      let newEdges' = Vector.update edges (Vector.singleton (index, disabledEdge))
      let newEdges = newEdges' Vector.++ Vector.fromList [edgeToNew, edgeFromNew]

      return $ Genome {nodes = newNodes, edges = newEdges}

-- TODO @incomplete: reuse GIN if the same mutation already occurred in the current generation
mutateAddEdge :: Genome -> TVar GIN -> (Float, Float) -> IO Genome
mutateAddEdge old@Genome{nodes, edges} ginVar weightRange =
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

mutateDisable :: (Edge, Maybe Edge) -> P -> Edge -> IO Edge
mutateDisable precondition disableP old =
  if predicate
    then do
      -- NB: the old enable status is completely ignored
      shouldDisable <- Random.trigger disableP
      let new = if shouldDisable
                  then old{enableStatus = Disabled}
                  else old{enableStatus = Enabled}
      return new
    else
      return old
  where
    predicate =
      case precondition of
        (Edge{enableStatus=Enabled}, _) -> True
        (Edge{enableStatus=Disabled}, Nothing) -> False
        (Edge{enableStatus=Disabled}, Just Edge{enableStatus=Enabled}) -> True
        (Edge{enableStatus=Disabled}, Just Edge{enableStatus=Disabled}) -> False


-- | Cross over two genomes to get a new one.
--
-- At the matching position, the random one is chosen.
-- At the mismatch position:
--   If there is a more fit genome, the disjoint and excess genes are always chosen from it.
--   If the fitnesses are equal, then genes from both are included.
crossover :: P -> (Genome, AdjustedFitness) -> (Genome, AdjustedFitness) -> IO Genome
crossover disableP (left, AdjustedFitness leftFitness) (right, AdjustedFitness rightFitness) = do
  let alignedEdges = zipEdges (edges left) (edges right)
  -- this is a list of random bools used to break ties
  triggers <- Random.triggers (length alignedEdges) (P 0.5)

  let winner = case compare leftFitness rightFitness of
        EQ -> Nothing
        LT -> Just (Right ())
        GT -> Just (Left ())

  let pickLeft edge precondition = do
        newEdge <- mutateDisable precondition disableP edge
        return (newEdge, [nodes left Map.! inNodeId edge, nodes left Map.! outNodeId edge])

  let pickRight edge precondition = do
        newEdge <- mutateDisable precondition disableP edge
        return (newEdge, [nodes right Map.! inNodeId edge, nodes right Map.! outNodeId edge])

  (finalEdges, dupNodes) <-
    zip triggers alignedEdges
      |> map (\case
           (isLeft, Both l r) ->
             let precond = (l, Just r)
             in Just $ if isLeft then pickLeft l precond else pickRight r precond
           (_, Sinistra _ l) ->
             case winner of
               -- if there is no winner, include both
               Nothing -> Just $ pickLeft l (l, Nothing)
               -- otherwise, always choose from the winner
               Just (Left ()) -> Just $ pickLeft l (l, Nothing)
               Just (Right ()) -> Nothing
           (_, Destra _ r) ->
             case winner of
               -- if there is no winner, include both
               Nothing -> Just $ pickRight r (r, Nothing)
               -- otherwise, always choose from the winner
               Just (Left ()) -> Nothing
               Just (Right ()) -> Just $ pickRight r (r, Nothing))
      |> filter Data.Maybe.isJust |> map Data.Maybe.fromJust
      |> sequence
      |> fmap unzip

  let finalNodes = foldr (\node acc -> Map.insert (nodeId node) node acc) Map.empty (concat dupNodes)

  let child = Genome
        { nodes = finalNodes
        , edges = Vector.fromList finalEdges
        }

  return child

-- | This is the equation (1) in the paper.
compatibility :: (Float, Float, Float) -> Genome -> Genome -> Float
compatibility (c1, c2, c3) Genome{edges=edgesA} Genome{edges=edgesB} =
  c1 * nExcesses / n + c2 * nDisjoints / n + c3 * meanDiff
  where
    n' = max (Vector.length edgesA) (Vector.length edgesB)
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


-- | Reproduce a species.
--
-- Implemented:
--   - top r% is allowed to crossover (r% is computed at the caller)
--   - champion is copied without mutation
-- Not Implemented: TODO @incomplete: implement this
--   - 25% of the offspring result from mutation without crossover
--   - 0.1% chance of interspecies mating
reproduceSpecies :: (Float, Float) -> MutateParams -> TVar GIN
                 -> SpeciesWithAdjustedFitness -> Int
                 -> IO Species
reproduceSpecies
  weightRange mutateParams ginVar
  gf@(SpeciesWithAdjustedFitness genomeWithFitnesses) numOffsprings
    | numOffsprings == 0 = return $ Species Vector.empty
    | Vector.null genomeWithFitnesses = return $ Species Vector.empty
    | otherwise = do
      -- mate the top fitted members
      tops <- Vector.replicateM numOffsprings $ do
        -- since genomeWithFitnesses is not empty, this is guaranteed to be a Just
        Just parent1 <- Random.chooseWith getWeight genomeWithFitnesses
        Just parent2 <- Random.chooseWith getWeight genomeWithFitnesses
        offspring <- crossover (geneDisableP mutateParams) parent1 parent2
        mutateGenome weightRange mutateParams ginVar offspring

      -- copy the champion to the next generation without mutation
      -- TODO @incomplete: more aggressive elitism?
      let champions =
            -- TODO @incomplete: do not hard code the 5
            if Vector.length genomeWithFitnesses > 5
              then
                case getChampion gf of
                  Nothing -> Vector.empty
                  Just champion -> Vector.singleton champion
              else
                Vector.empty

      -- maybe reverse the order of the concatenation is more performant,
      -- but this may lead to a bad choice of representative of the species,
      -- if later we choose the first genome to be the representative of the species,
      -- we will worry about the performance later
      return . Species $ tops Vector.++ champions
      where
        -- sample uniformly
        getWeight = const 1.0
        -- sample according to fitness
        -- getWeight (_genome, AdjustedFitness weight) = weight


mutateGenome :: (Float, Float) -> MutateParams -> TVar GIN -> Genome -> IO Genome
mutateGenome
  weightRange
  MutateParams{mutateWeightP, perturbeWeightP, perturbeRange, mutateAddNodeP, mutateAddEdgeP}
  ginVar
  old@Genome{edges} = do
    -- mutation: weight of existing edge
    newEdges <- Vector.mapM
      (mutateExistingEdge mutateWeightP perturbeWeightP perturbeRange weightRange)
      edges
    let new' = old{edges = newEdges}

    -- mutation: add node
    shouldAddNode <- Random.trigger mutateAddNodeP
    new'' <-
      if shouldAddNode
        then mutateAddNode new' ginVar
        else return new'

    -- mutation: add edge
    shouldAddEdge <- Random.trigger mutateAddEdgeP
    new''' <-
      if shouldAddEdge
        then mutateAddEdge new'' ginVar weightRange
        else return new''

    return new'''


-- TODO @incomplete: is this efficient?
getChampion :: SpeciesWithAdjustedFitness -> Maybe Genome
getChampion (SpeciesWithAdjustedFitness xs) =
  Vector.toList xs
    |> sortBy (\(_, AdjustedFitness w1) (_, AdjustedFitness w2) -> compare w1 w2)
    |> headMay
    |> fmap fst


-- | original_fitness / population_of_this_species
adjustFitness :: (Genome -> OriginalFitness) -> Species -> SpeciesWithAdjustedFitness
adjustFitness fitness (Species genomes)
  | Vector.null genomes = SpeciesWithAdjustedFitness Vector.empty
  | otherwise =
    let numGenomes = fromIntegral $ Vector.length genomes
        fitness' g =
          let OriginalFitness f = fitness g
          -- since genomes is not empty, the division is safe
          in AdjustedFitness $ f / numGenomes
        species = Vector.map (\g -> (g, fitness' g)) genomes
    in SpeciesWithAdjustedFitness species


-- | Speciate the old generation into a new generation according to compatibility.
speciate :: CompatibilityParams -> Generation -> IO Generation
speciate compatibilityParams (Generation oldGen) = do
  zero <- Vector.mapM chooseSpeciesRepr oldGen
  let oldPopulation = oldGen
        |> Vector.map (\(Species s) -> s)  -- :: Vector (Vector Genome)
        |> Vector.toList          -- :: [Vector Genome]
        |> Vector.concat          -- :: Vector Genome
  foldl' (\acc idv ->
    case Vector.findIndex (\withRepr -> isSameSpecies withRepr idv) acc of
      Nothing ->
        -- a new species is created, and appended to the end,
        -- with the first member as its representative
        Vector.snoc acc $ Just (idv, Vector.singleton idv)
      Just i ->
        -- since we just found i, access to the i-th element is safe
        -- by the way the prediction works, it has to be a Just
        let Just (oldRepr, oldSpecies) = acc Vector.! i
            newSpecies = Vector.snoc oldSpecies idv
        in acc Vector.// [(i, Just (oldRepr, newSpecies))]
    ) zero oldPopulation
    |> Vector.map (\case
         Nothing -> Vector.empty
         Just (_, x) -> x)
    |> Vector.map Species
    |> Generation
    |> return
  where
    chooseSpeciesRepr (Species genomes) = do
      chosen' <- Random.chooseUniformly genomes
      case chosen' of
        Nothing -> return Nothing
        Just chosen -> return $ Just (chosen, Vector.empty)

    isSameSpecies Nothing _ = False
    isSameSpecies (Just (a, _)) b =
      compatibility (c123 compatibilityParams) a b <= threshold compatibilityParams


-- | Evolve: from the current generation to the next generation
evolve :: (Genome -> OriginalFitness) -> CompatibilityParams
       -> (Float, Float) -> MutateParams -> TVar GIN
       -> Generation -> IO Generation
evolve
  fitness compatibilityParams
  weightRange mutateParams ginVar
  (Generation prevGen) = do
    -- the adjusted fitness
    let speciesWithFitnesses = Vector.map (adjustFitness fitness) prevGen

    -- number of offsprings for each species
    let speciesTotalFitnesses = speciesWithFitnesses
          |> Vector.map (\(SpeciesWithAdjustedFitness s) -> s)
          |> Vector.map (Vector.sum . Vector.map ((\(AdjustedFitness f) -> f) . snd))
    let speciesSizes = Vector.map (Vector.length . (\(Species x) -> x)) prevGen
    let globalMeanFitness = Vector.sum speciesTotalFitnesses / fromIntegral (Vector.sum speciesSizes)
    let newSpeciesSizes = Vector.map (ceiling . (/ globalMeanFitness)) speciesTotalFitnesses

    -- reproduce
    newGen' <- Vector.mapM (uncurry $ reproduceSpecies weightRange mutateParams ginVar) (Vector.zip speciesWithFitnesses newSpeciesSizes)
    newGen <- speciate compatibilityParams (Generation newGen')

    return newGen


makeInitPopulation :: Params -> TVar GIN -> IO Population
makeInitPopulation Params{numInitPopulation, weightRange, numInNodes, numOutNodes} ginVar = do
  genomes <- Vector.generateM numInitPopulation (\_ -> makeInitGenome weightRange numInNodes numOutNodes ginVar)
  return $ Population genomes

simulate :: (Genome -> OriginalFitness) -> CompatibilityParams
         -> (Float, Float) -> MutateParams -> TVar GIN
         -> Int -> Population
         -> Path Abs Dir -> IO Generation
simulate
  fitness compatibilityParams
  weightRange mutateParams ginVar
  numGenerations (Population initPopulation)
  visDir = do
    let initGen = Generation . Vector.singleton . Species $ initPopulation
    visOneGen fitness visDir 0 initGen
    foldM (\prevGen genNum -> do
      newGen <- evolve fitness compatibilityParams weightRange mutateParams ginVar prevGen
      visOneGen fitness visDir genNum newGen
      return newGen
      ) initGen [1 .. numGenerations]


-- TODO @incomplete: these fromJust looks dirty
nodeValue :: Node -> Genome -> [Float] -> Float
nodeValue n@Node{kind=Sensor} Genome{nodes} sensorValues =
  let nodesAsc = Map.toAscList nodes
        |> map snd
        |> filter ((== Sensor) . kind)
      index = fromJust $ elemIndex n nodesAsc
  in sensorValues !! index
nodeValue Node{nodeId} genome@Genome{nodes, edges} sensorValues =
  Vector.filter (\edge -> outNodeId edge == nodeId) edges
    |> Vector.map (\edge -> weight edge * nodeValue (getInNode edge) genome sensorValues)
    |> Vector.sum
  where
    getInNode edge = fromJust $
      Map.lookup (inNodeId edge) nodes

genomeValue :: Genome -> [Float] -> [Float]
genomeValue genome@Genome{nodes} sensorValues =
  Map.toAscList nodes
    |> filter (\(_, node) -> kind node == Output)
    |> map (\(_, n) -> nodeValue n genome sensorValues)

