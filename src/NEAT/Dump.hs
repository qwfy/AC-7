{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Dump
  ( createRun
  , createGeneration
  , createSpecies
  )
  where

import NEAT.Data
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Database.Bolt as Bolt
import Data.UUID (UUID)

newtype GenerationSn = GenerationSn Int
newtype SpeciesSn = SpeciesSn Int

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
      , "return id(run) as runNodeId"']
    params = Map.fromList [("runId", Bolt.T runId)]


createGeneration :: RunNodeId -> GenerationSn -> IO GenerationNodeId
createGeneration (RunNodeId runNodeId) (GenerationSn generationSn)
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
