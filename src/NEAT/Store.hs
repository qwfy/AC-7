{-
Description: Dump a run to Neo4j

The database is designed as follows:

- Each run is identified by a globally unique run id
- A run has many generations
- Each generation is identified by a sequential number, which is local to the run
- A generation has many species
- Each species is identified by a sequential number, which is local to the generation
- A species has many genomes
- Each genome is identified by a globally unique genome id
- A genome has many nodes and edges
- Each node is identified by a globally unique node id
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module NEAT.Store
  ( createConstraint
  , createRun
  , storeGeneration
  , GenerationSn(..)
  )
  where

import NEAT.Data
import qualified Data.Map.Strict as Map
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Database.Bolt as Bolt
import Data.Default
import Data.UUID (UUID)
import Data.UUID.V4
import qualified Data.Vector as Vector
import Data.String
import Random

import Data.AC7

newtype GenerationSn = GenerationSn Int deriving (Show)
newtype SpeciesSn = SpeciesSn Int deriving (Show)
newtype GenomeId = GenomeId UUID deriving (Show)

newtype RunNodeId = RunNodeId Bolt.Value
newtype GenerationNodeId = GenerationNodeId Bolt.Value
newtype SpeciesNodeId = SpeciesNodeId Bolt.Value
newtype GenomeNodeId = GenomeNodeId Bolt.Value

createConstraint :: Bolt.Pipe -> IO ()
createConstraint pipe =
  mapM_ (Bolt.run pipe . Bolt.query_) constraints
  where
    constraints =
      [ "create constraint on (run:Run) assert run.runId is unique"
      , "create constraint on (genome:Genome) assert genome.genomeId is unique"
      , "create constraint on (node:Node) assert node.nodeId is unique"

      , "create constraint on (run:Run) assert exists(run.runId)"
      , "create constraint on (generation:Generation) assert exists(generation.generationSn)"
      , "create constraint on (species:Species) assert exists(species.speciesSn)"
      , "create constraint on (genome:Genome) assert exists(genome.genomeId)"
      , "create constraint on (genome:Genome) assert exists(genome.originalFitness)"
      , "create constraint on (node:Node) assert exists(node.nodeId)"
      , "create constraint on (node:Node) assert exists(node.kind)"
      ]

createRun :: Bolt.Pipe -> RunId -> IO RunNodeId
createRun pipe runId = do
  [record] <- Bolt.run pipe $ Bolt.queryP query params
  RunNodeId <$> Bolt.at record "runNodeId"
  where
    query = Text.unwords
      [ "create (run:Run {runId:$runId})"
      , "return id(run) as runNodeId"]
    params = Map.fromList
      [ ("runId", Bolt.T . fromString . show $ runId)]


-- | Write one generation to the run identified by 'RunNodeId'.
storeGeneration :: Bolt.Pipe -> (Genome -> OriginalFitness) -> RunNodeId -> GenerationSn -> Generation -> IO ()
storeGeneration pipe fitness runNodeId generationSn (Generation generation) = do
  generationNodeId <- createGeneration pipe runNodeId generationSn

  -- store each species in the generation
  flip Vector.imapM_ generation $ \speciesSn (Species species) -> do
    speciesNodeId <- createSpecies pipe generationNodeId (SpeciesSn speciesSn)

    --  store each genome in the species
    flip Vector.mapM_ species $ \genome -> do
      -- TODO @incomplete: this id is created locally, should we create this at the business level?
      genomeId <- GenomeId <$> Random.newGUID
      createGenome pipe fitness speciesNodeId genomeId genome

createGeneration :: Bolt.Pipe -> RunNodeId -> GenerationSn -> IO GenerationNodeId
createGeneration pipe (RunNodeId runNodeId) generationSn = do
  [record] <- Bolt.run pipe $ Bolt.queryP query params
  GenerationNodeId <$> Bolt.at record "generationNodeId"
  where
    query = Text.unwords
      [ "match (run:Run) where id(run) = $runNodeId"
      , "create (generation:Generation {generationSn:$generationSn}),"
      , "(generation) -[:GENERATION_OF]-> (run)"
      , "return id(generation) as generationNodeId"]
    params = Map.fromList
      [ ("runNodeId", runNodeId)
      , ("generationSn", Bolt.T . fromString . show $ generationSn)]


createSpecies :: Bolt.Pipe -> GenerationNodeId -> SpeciesSn -> IO SpeciesNodeId
createSpecies pipe (GenerationNodeId generationNodeId) speciesSn = do
  [record] <- Bolt.run pipe $ Bolt.queryP query params
  SpeciesNodeId <$> Bolt.at record "speciesNodeId"
  where
    query = Text.unwords
      [ "match (generation:Generation) where id(generation) = $generationNodeId"
      , "create (species:Species {speciesSn:$speciesSn}),"
      , "(species) -[:SPECIES_OF]-> (generation)"
      , "return id(species) as speciesNodeId"]
    params = Map.fromList
      [ ("generationNodeId", generationNodeId)
      , ("speciesSn", Bolt.T . fromString . show $ speciesSn)]


-- | Create a genome identified by 'GenomeId' and belongs to the species identified by 'SpeciesNodeId'.
createGenome :: Bolt.Pipe -> (Genome -> OriginalFitness) -> SpeciesNodeId -> GenomeId -> Genome -> IO ()
createGenome pipe fitness (SpeciesNodeId speciesNodeId) genomeId genome@Genome{nodes, edges} = do
  [genomeVirtual] <- Bolt.run pipe $ uncurry Bolt.queryP createGenomeVirtual
  genomeNodeId <- GenomeNodeId <$> Bolt.at genomeVirtual "genomeId"
  let nodeQueries = map (createNode genomeNodeId) (Map.elems nodes)
  let edgeQueries = map createEdge (Vector.toList edges)
  mapM_ (Bolt.run pipe . uncurry Bolt.queryP_) (nodeQueries ++ edgeQueries)
  where
    -- Create the genome node, this is where the information of the genome,
    -- such as fitness, is stored.
    createGenomeVirtual :: (Text, Map Text Bolt.Value)
    createGenomeVirtual =
      let query = Text.unwords
            [ "match (species:Species) where id(species) = $speciesNodeId"
            , "create (genome:Genome {genomeId:$genomeId, originalFitness:$originalFitness}),"
            , "(genome) -[:GENOME_OF]-> (species)"
            , "return id(genome) as genomeId"]
          OriginalFitness originalFitness = fitness genome
          params = Map.fromList
            [ ("speciesNodeId", speciesNodeId)
            , ("genomeId", Bolt.T . fromString . show $ genomeId)
            , ("originalFitness", Bolt.F . realToFrac $ originalFitness)]
      in (query, params)

    createNode :: GenomeNodeId -> Node -> (Text, Map Text Bolt.Value)
    createNode (GenomeNodeId genomeNodeId) Node{nodeId, kind} =
      let query = Text.unwords
            [ "match (genome:Genome) where id(genome) = $genomeNodeId"
            , "create (node:Node {nodeId:$nodeId, kind:$kind}),"
            , "(node) -[:NODE_OF]-> (genome)"]
          params = Map.fromList
            -- TODO @incomplete: should we rely on the show instance?
            [ ("genomeNodeId", genomeNodeId)
            , ("nodeId", Bolt.T . fromString . show $ nodeId)
            , ("kind", Bolt.T . fromString . show $ kind)]
      in (query, params)

    -- TODO @incomplete: this implementation relies on the fact that
    -- the node ids are globally unique, this can be fixed
    createEdge :: Edge -> (Text, Map Text Bolt.Value)
    createEdge Edge{inNodeId, outNodeId, weight, enableStatus, gin} =
      let query = Text.unwords
            [ "match (inNode:Node) where inNode.nodeId = $inNodeId"
            , "match (outNode:Node) where outNode.nodeId = $outNodeId"
            , "create (inNode) -[:INPUT_OF {weight:$weight, enableStatus:$enableStatus, gin:$gin}]-> (outNode)"]
          params = Map.fromList
            [ ("inNodeId", Bolt.T . fromString . show $ inNodeId)
            , ("outNodeId", Bolt.T . fromString . show $ outNodeId)
            , ("weight", Bolt.F . realToFrac $ weight)
            , ("enableStatus", Bolt.T . fromString . show $ enableStatus)
            , ("gin", Bolt.T . fromString . show $ gin)]
      in (query, params)
