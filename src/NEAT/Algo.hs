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
import GHC.Exts
import qualified Data.Vector as Vector
import Data.Vector (Vector)
import qualified System.Random
import qualified Random
import Random (P(..))
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM (STM, TVar)
import Control.Monad
import Safe
import qualified Database.Bolt as Bolt
import Control.Exception
import Control.Concurrent.ProcessPool

import Data.AC7
import NEAT.Data
import NEAT.Store
import GraphDb
import Util


-- TODO @incomplete: Things to consider:
--
-- - Interspecies breeding
-- https://stats.stackexchange.com/questions/272901/do-different-species-create-offspring-neat
-- This depends mostly on whether you want it to or not. Stanley and Miikkulainen do leave this pretty open, but in the NEAT C++ code which accompanies the paper, there is a parameter named interspecies_mate_rate which is often very low (around %0.1). Also, there is the parameter for stolen_babies which is usually disabled, but would also introduce interspecies breeding.
-- So to answer your question, you can get away with no interspecies breeding, but the theory of adding it, is that it could allow allow the best of both species to merge.
-- (It also models what occasionally occurs in nature, and is an interesting consideration)


makeGINTVar :: GIN -> IO (TVar GIN)
makeGINTVar = STM.newTVarIO

increaseGIN :: TVar GIN -> STM GIN
increaseGIN var = do
  GIN oldV <- STM.readTVar var
  let new = GIN (oldV + 1)
  STM.writeTVar var new
  return new

makeNode :: NodeKind -> IO Node
makeNode kind = do
  nodeId <- Random.newGUID
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
  inNodes <- mapM (makeNode . Sensor) [0 .. numInNodes - 1]
  outNodes <- mapM (makeNode . Output) [0 .. numOutNodes - 1]
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
-- TODO @incomplete: Hebbian?
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
      notInputIds = nodes
        |> Map.filter (not . isSensorNode)
        |> Map.keys
      -- NB: #ways-to-connect-nodes#
      -- connections to Sensor nodes are disallowed
      -- connections from Output nodes are allowed
      -- recurrent connections are allowed
      allPairs = [(f, t) | f <- nodeIds, t <- notInputIds]
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


-- TODO @incomplete: do not disable sensor edges, or should we?
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


-- | NB: This function relies on the fact that the edges are monotonically increasing.
-- TODO @incomplete: should we remove this assumption?
zipEdges :: Vector Edge -> Vector Edge -> [Trither Edge Edge]
zipEdges lefts rights =
  case (Vector.null lefts, Vector.null rights) of
    (True, True) -> []
    (True, False) -> map (Destra Excess) (Vector.toList rights)
    (False, True) -> map (Sinistra Excess) (Vector.toList lefts)
    (False, False) ->
      let (_, _, reversed) = foldl' (
            \acc@(lIndex, rIndex, accEdges) _ ->
              case (lIndex, rIndex) of
                (Nothing, Nothing) -> acc
                (ll@(Just li), rr@Nothing) -> (increaseL ll, rr, Sinistra Excess (lefts Vector.! li) : accEdges)
                (ll@Nothing, rr@(Just ri)) -> (ll, increaseR rr, Destra Excess (rights Vector.! ri) : accEdges)
                (ll@(Just li), rr@(Just ri)) ->
                  let left = lefts Vector.! li
                      right = rights Vector.! ri
                  in case compare (getGin left) (getGin right) of
                       -- historical markings match
                       -- 1 2 3
                       -- 1 2 3
                       EQ -> (increaseL ll, increaseR rr, Both left right : accEdges)
                       -- 1 2 3 4 8
                       -- 1 2     8
                       LT -> (increaseL ll, rr, Sinistra Disjoint left : accEdges)
                       -- 1 2     8
                       -- 1 2 3 4 8
                       GT -> (ll, increaseR rr, Destra Disjoint right : accEdges)
            ) (Just 0, Just 0, []) [1 .. maxLGin + maxRGin]
      in reverse reversed
  where
    getGin Edge{gin=GIN x} = x
    maxLGin = Vector.maximum $ Vector.map getGin lefts
    maxRGin = Vector.maximum $ Vector.map getGin rights
    increase _ Nothing = Nothing
    increase ubi (Just i) =
      if i < ubi
        then Just (i + 1)
        else Nothing
    increaseL = increase (Vector.length lefts - 1)
    increaseR = increase (Vector.length rights - 1)


-- | Cross over two genomes to get a new one.
--
-- At the matching position, the random one is chosen.
-- At the mismatch position:
--   If there is a more fit genome, the disjoint and excess genes are always chosen from it.
--   If the fitnesses are equal, then genes from both are included.
--
-- TODO @incomplete: this may produce ill-formed offsprings
-- TODO @incomplete: link deduplicaiton
-- TODO @incomplete: are Sensor and Output nodes always aligned?
crossover :: P -> (Genome, AdjustedFitness) -> (Genome, AdjustedFitness) -> IO Genome
crossover disableP (left', AdjustedFitness leftFitness) (right', AdjustedFitness rightFitness) = do
  (left, right) <- unifyBoundaryNodeId left' right'

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

  child <- replaceNodeId $ Genome
    { nodes = finalNodes
    , edges = Vector.fromList finalEdges}

  return child


unifyBoundaryNodeId :: Genome -> Genome -> IO (Genome, Genome)
unifyBoundaryNodeId p1@Genome{nodes=nodes1} p2@Genome{nodes=nodes2} = do
  let numInputs = max (countInputs nodes1) (countInputs nodes2)
  let numOutputs = max (countOutputs nodes1) (countOutputs nodes2)
  newInputIds <- Vector.replicateM numInputs Random.newGUID
  newOutputIds <- Vector.replicateM numOutputs Random.newGUID
  return (replace newInputIds newOutputIds p1, replace newInputIds newOutputIds p2)

  where

    countInputs nodes = nodes
      |> Map.filter isSensorNode
      |> Map.size

    countOutputs nodes = nodes
      |> Map.filter isOutputNode
      |> Map.size

    replace newInputIds newOutputIds g@Genome{nodes, edges} =
      let (newNodes', (inLookup, outLookup)) =
            mapFold (Map.elems nodes) ([], []) (\n@Node{nodeId=NodeId oldNodeId, kind} acc@(accIns, accOuts) ->
              case kind of
                Sensor i ->
                  let newNodeId = newInputIds Vector.! i
                  in (n{nodeId=NodeId newNodeId}, ((oldNodeId, newNodeId) : accIns, accOuts))
                Hidden ->
                  (n, acc)
                Output i ->
                  let newNodeId = newOutputIds Vector.! i
                  in (n{nodeId=NodeId newNodeId}, (accIns, (oldNodeId, newNodeId) : accOuts)))
          newNodes = Map.fromList (map (\n -> (nodeId n, n)) newNodes')

          newEdges = edges
            |> Vector.map (\e@Edge{inNodeId=NodeId inNodeId, outNodeId=NodeId outNodeId} ->

                 let -- Output node can also be in the position of inNodeId
                     -- but Sensor node can not be in the position of outNodeId
                     -- see #ways-to-connect-nodes
                     newInNodeId = keyGet inNodeId (inLookup ++ outLookup) inNodeId
                     newOutNodeId = keyGet outNodeId outLookup outNodeId
                in e{inNodeId=NodeId newInNodeId, outNodeId=NodeId newOutNodeId})
      in g{nodes=newNodes, edges=newEdges}


mapFold :: [x] -> zero -> (x -> zero -> (y, zero)) -> ([y], zero)
mapFold xs zero f =
  let (revYs, finalAcc) = foldl' g ([], zero) xs
  in (reverse revYs, finalAcc)
  where
    g (accYs, accZero) x =
      let (y, newZero) = f x accZero
      in (y : accYs, newZero)


keyFind :: Eq a => a -> [(a, b)] -> Maybe b
keyFind k kvs =
  fmap snd (find test kvs)
  where
    test kv = fst kv == k


keyGet :: Eq a => a -> [(a, b)] -> b -> b
keyGet k kvs def =
  case keyFind k kvs of
    Nothing -> def
    Just v -> v


-- | An offspring should have completely new node ids,
-- for there is an UNIQUE constraint in the database.
-- This function replace all node ids in the 'old' genome with newly created ones.
replaceNodeId :: Genome -> IO Genome
replaceNodeId old@Genome{nodes, edges} = do
  newIds <- replicateM (Map.size nodes) (NodeId <$> Random.newGUID)
  let (oldIds, oldNodes) = unzip . Map.assocs $ nodes
  let newNodes = zip newIds oldNodes
        |> map (\(newId, node) -> (newId, node{nodeId=newId}))
        |> Map.fromList
  let table = Map.fromList $ zip oldIds newIds
  let newEdges = flip Vector.map edges (\edge@Edge{inNodeId, outNodeId} ->
        let newInNodeId = table Map.! inNodeId
            newOutNodeId = table Map.! outNodeId
        in edge{inNodeId = newInNodeId, outNodeId = newOutNodeId})
  return $ old{nodes=newNodes, edges=newEdges}


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
      let champions' =
            -- TODO @incomplete: do not hard code the 5
            if Vector.length genomeWithFitnesses > 5
              then
                case getChampion gf of
                  Nothing -> Vector.empty
                  Just champion -> Vector.singleton champion
              else
                Vector.empty
      champions <- Vector.mapM replaceNodeId champions'

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
        |> Vector.toList                   -- :: [Vector Genome]
        |> Vector.concat                   -- :: Vector Genome
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
    chooseSpeciesRepr :: Species -> IO (Maybe (Genome, Vector Genome))
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

simulate :: RunId
         -> (Genome -> OriginalFitness) -> CompatibilityParams
         -> (Float, Float) -> MutateParams -> TVar GIN
         -> Int -> Population
         -> IO Generation
simulate
  runId
  fitness compatibilityParams
  weightRange mutateParams ginVar
  numGenerations (Population initPopulation) = do
    pipe <- Bolt.connect GraphDb.config
    runNodeId <- NEAT.Store.createRun pipe runId

    pool <- startPool 20

    putStrLn "creating the initial generation"
    let initGen = Generation . Vector.singleton . Species $ initPopulation

    persistentJob0 <- submitJob pool $
      NEAT.Store.storeGeneration pipe fitness runNodeId (GenerationSn 0) initGen

    (finalGen, persistentJobs) <- foldM (\(prevGen, accJobs) genSn -> do
      putStrLn $ "creating generation " ++ show genSn
      newGen <- evolve fitness compatibilityParams weightRange mutateParams ginVar prevGen

      persistentJob <- submitJob pool $ do
        pipe' <- Bolt.connect GraphDb.config
        NEAT.Store.storeGeneration pipe' fitness runNodeId (GenerationSn genSn) newGen

      return (newGen, persistentJob : accJobs)
      ) (initGen, [persistentJob0]) [1 .. numGenerations]

    putStrLn "simulation complete, waiting for the peristent jobs to finish"
    forM_ (reverse persistentJobs) (\job -> do
      putStrLn $ "waiting for job " ++ show job
      waitJob pool job)

    return finalGen


isSensorNode :: Node -> Bool
isSensorNode Node{kind} =
  case kind of
    Sensor _ -> True
    Hidden   -> False
    Output _ -> False

isOutputNode :: Node -> Bool
isOutputNode Node{kind} =
  case kind of
    Sensor _ -> False
    Hidden   -> False
    Output _ -> True

sigmoid x =
  -- this value is copied from NEAT-C/neat.cpp
  let slope = 4.924273
  in 1 / (1 + exp(- slope * x))

-- | This is used to handle recurrent connections.
-- If 'esCounter' > 0, then the corresponding node is being evaluated,
-- thus, to prevent recursion, any subsequent access of the node's value
-- should read directly from 'esValue'.
-- The 'esCounter' should always be greater than or equal to zero.
data EvaluationStatus = EvaluationStatus
  { esCounter :: Int
  , esValue :: Float
  }

type Env = Map NodeId EvaluationStatus

-- | Increase the 'esCounter' before passing the 'Env' to be evaluated,
-- decrease it when the evaluation is done.
withOpenNode :: Env -> NodeId -> (Env -> (a, Env)) -> (a, Env)
withOpenNode env' nodeId f =
  let (x, newEnv) = f (increase env')
  in (x, decrease newEnv)
  where
    increase env =
      case Map.lookup nodeId env of
        Nothing ->
          let newEs = EvaluationStatus{esCounter=1, esValue=0.0}
          in Map.insert nodeId newEs env
        Just es@EvaluationStatus{esCounter} ->
          let newEs = assert (esCounter >= 0) es{esCounter=esCounter + 1}
          in Map.insert nodeId newEs env
    decrease env =
      -- The "Map.!" is safe, since we just increased nodeId
      let es@EvaluationStatus{esCounter} = env Map.! nodeId
          newEs = assert (esCounter >= 1) es{esCounter=esCounter-1}
      in Map.insert nodeId newEs env


nodeValue :: [Float] -> Genome -> Node -> Env -> (Float, Env)

nodeValue sensorValues _genome Node{kind=Sensor index} env =
  -- If it is a Sensor node, we directly read its value.
  (sensorValues !! index, env)

nodeValue sensorValues genome@Genome{nodes, edges} Node{nodeId=targetNodeId} env' =
  withOpenNode env' targetNodeId calculate
  where
    calculate env =
      edges
        -- All enabled incoming edges
        |> Vector.filter (\Edge{outNodeId, enableStatus} ->
             enableStatus == Enabled && outNodeId == targetNodeId)
        -- TODO @incomplete: Sort to get deterministic evaluation order
        -- TODO @incomplete: A better sort algorithm, e.g. sort by distance (hops) to the Sensors
        |> Vector.foldl' accumulateFromEdge (0.0, env)
        |> (\(z, accEnv) ->
             let newZ = sigmoid z
                 newEnv = updateEsValue targetNodeId newZ accEnv
             in (newZ, newEnv))

    -- TODO @incomplete: Is this faithful to the original implementation?
    -- TODO @incomplete: Add bias
    -- This calculates
    --   sum_i(w_i * x_i)
    -- where
    --   sum_i means sum over i
    --   i is index to incoming edges of targetNodeId
    --   w_i is the edge's weight
    --   x_i is value of the incoming node of the edge
    accumulateFromEdge (accSum, accEnv) Edge{inNodeId, weight} =
      let (inNodeValue, newAccEnv) =
            case Map.lookup inNodeId accEnv of
              Just EvaluationStatus{esCounter, esValue} ->
                -- If the incoming node is currently "open" (means esCounter > 0),
                -- then we just return its stored esValue, this prevents recursion.
                -- Otherwise, we recursively call the nodeValue function on the incoming
                -- node to calculate the value of the incoming node. Note that, since we
                -- are calculating the summation of the value of the incoming node of the
                -- incoming edges, as long as the there are no recurrent connections, we
                -- won't access the target node's esValue, so the openness of the target
                -- node doesn't affect the summation.
                if esCounter > 0
                  then (esValue, accEnv)
                  else nodeValue sensorValues genome (nodes Map.! inNodeId) accEnv
              Nothing ->
                -- This indeed can happen: we may never encounter this node (the one in
                -- the argument) before.
                nodeValue sensorValues genome (nodes Map.! inNodeId) accEnv
      in (accSum + weight * inNodeValue, newAccEnv)

    updateEsValue nodeId' esValue env =
      let es = env Map.! nodeId'
          newEs = es{esValue=esValue}
      in Map.insert nodeId' newEs env


genomeValue :: Genome -> [Float] -> [Float]
genomeValue genome@Genome{nodes} sensorValues =
  nodes
    |> Map.filter isOutputNode
    |> Map.elems
    -- Evaluate the Output node with the smallest index first,
    -- so that the evaluation order is deterministic
    |> sortWith (\Node{kind=Output index} -> index)
    -- Since the Output nodes are in the same genome, they are somehow related,
    -- it is for this reason that we let the calculation of the value of the next
    -- Output node access the environment the previous node.
    -- However this will make the calculation sequential, which maybe bad for performance.
    -- TODO @incomplete: experimenting with map (instead of foldl), if they behave the
    -- same, then prefer map, since it can be parallelized.
    |> flip foldl' ([], Map.empty) (\(accOutputValues, accEnv) node ->
         let (outputValue, newAccEnv) = nodeValue sensorValues genome node accEnv
         in (outputValue:accOutputValues, newAccEnv))
    |> reverse . fst
