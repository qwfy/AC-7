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

module NEAT.Dump
  ( createRun
  , createGeneration
  , createSpecies
  )
  where

import NEAT.Data
import qualified Data.Map.Strict as Map
import Data.Map (Map)
import Data.Text (Text)
import qualified Database.Bolt as Bolt
import Data.UUID (UUID)

newtype RunId = RunId Text
newtype GenerationSn = GenerationSn Int
newtype SpeciesSn = SpeciesSn Int
newtype GenomeId = GenomeId UUID

newtype RunNodeId = RunNodeId Bolt.Value
newtype GenerationNodeId = GenerationNodeId Bolt.Value
newtype SpeciesNodeId = SpeciesNodeId Bolt.Value
newtype GenomeNodeId = GenomeNodeId Bolt.Value

createRun :: Text -> IO RunNodeId
createRun runId = do
  record:[] <- Bolt.run $ Bolt.queryP query params
  RunNodeId <$> Bolt.at record "runNodeId"
  where
    query = unwords
      -- TODO @incomplete: what is "create unique"
      [ "create (run:Run {runId:$runId})"
      , "return id(run) as runNodeId"]
    params = Map.fromList [("runId", Bolt.T runId)]


createGeneration :: RunNodeId -> GenerationSn -> IO GenerationNodeId
createGeneration (RunNodeId runNodeId) (GenerationSn generationSn) = do
  record:[] <- Bolt.run $ Bolt.queryP query params
  GenerationNodeId <$> Bolt.at record "generationNodeId"
  where
    query = unwords
      [ "match (run:Run) where id(run) = $runNodeId"
      , "create (generation:Generation {generationSn:$generationSn}),"
      , "(generation) -[:GENERATION_OF]-> (run)"
      , "return id(generation) as generationNodeId"]
    params = Map.fromList
      [ ("runNodeId", runNodeId)
      , ("generationSn", Bolt.I generationSn)]


createSpecies :: GenerationNodeId -> SpeciesSn -> IO SpeciesNodeId
createSpecies (GenerationNodeId generationNodeId) (SpeciesSn speciesSn) = do
  record:[] <- Bolt.run $ Bolt.queryP query params
  SpeciesNodeId <$> Bolt.at record "speciesNodeId"
  where
    query = unwords
      [ "match (generation:Generation) where id(generation) = $generationNodeId"
      , "create (species:Species) {speciesSn:$speciesSn},"
      , "(species) -[:SPECIES_OF]-> (generation)"
      , "return id(species) as speciesNodeId"]
    params = Map.fromList
      [ ("generationNodeId", generationNodeId)
      , ("speciesSn", Bolt.I speciesSn)]


createGenome :: (Genome -> OriginalFitness) -> SpeciesNodeId -> GenomeId -> Genome -> IO ()
createGenome fitness (SpeciesNodeId speciesNodeId) (GenomeId genomeId) genome@Genome{nodes, edges} =
  let createNodes = map createNode nodes
      createEdges = map createEdge edges
  in mapM_ (Bolt.run . uncurry . Bolt.queryP_) (createNodes ++ createEdges)
  where
    createGenomeVirtual :: (Text, Map Text Bolt.Value)
    createGenomeVirtual =
      let query = intercalate ","
            [ "match (species:Species) where id(species) = $speciesNodeId"
            , "create (genome:Genome {genomeId:$genomeId, fitnness:fitness})"
            , "genome -[:GENOME_OF]-> species"]
          params = Map.fromList
            [ ("speciesNodeId", speciesNodeId)
            , ("genomeId", Bolt.T . fromString . show $ genomeId)
            , ("fitness", Bolt.F $ fitness genome)]
      in (query, params)

    createNode :: Node -> (Text, Map Text Bolt.Value)
    createNode Node{nodeId, kind} =
      let query = "create (node:Node $properties)"
          properties = Map.fromList
            -- TODO @incomplete: should we rely on the show instance?
            [ ("nodeId", Bolt.T . fromString . show $ nodeId)
            , ("kind", Bolt.T . fromString . show $ kind)
            , ("genomeId", Bolt.I . fromString . show $ genomeId)]
      in (query, properties)

    -- TODO @incomplete: this implementation relies on the fact that
    -- the node ids are globally unique
    createEdge :: Edge -> (Text, Map Text Bolt.Value)
    createEdge Edge{inNodeId, outNodeId, weight, enableStatus, gin=GIN gin} =
      let query = fromString $ unlines
            [ "match (inNode:Node) where inNode.nodeId = $inNodeId"
            , "match (outNode:Node) where outNode.nodeId = $outNodeId"
            , "inNode -[:INPUT_OF $properties]-> outNode"]
          properties = Map.fromList
            [ ("weight", Bolt.F weight)
            , ("enableStatus", Bolt.T . fromString . show $ enableStatus)
            , ("gin", Bolt.I gin)]
      in (query, properties)
