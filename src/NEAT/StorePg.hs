{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module NEAT.StorePg
  ( getConnection
  , createRun
  , finishRun
  , storeGeneration
  , GenerationSn(..)
  )
  where

import NEAT.Data
import qualified Data.Map.Strict as Map
import Data.UUID (UUID)
import qualified Data.Vector as Vector
import Random
import Util

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField (ToField, toField)

import Data.AC7

newtype GenerationSn = GenerationSn Int deriving (Show)
newtype SpeciesSn = SpeciesSn Int deriving (Show)
newtype GenomeId = GenomeId UUID deriving (Show)


-- TODO @incomplete: orphan instance?
instance ToField NodeId where
  toField (NodeId x) = toField x

instance ToField RunId where
  toField (RunId x) = toField x


getConnection :: IO Connection
getConnection = connect $ ConnectInfo
  { connectHost = "127.0.0.1"
  , connectPort = 5432
  , connectUser = "postgres"
  , connectPassword = "atreehasmanyleafs"
  , connectDatabase = "postgres"}


createRun :: Connection -> RunId -> IO ()
createRun conn runId = do
  current8601 <- iso8601DatetimeWithTimezone
  let sql = "\
        \insert into run \
        \(run_id, time_started) \
        \values (?, ?)"
  1 <- execute conn sql (runId, current8601)
  return ()


finishRun :: Connection -> RunId -> IO ()
finishRun conn runId = do
  current8601 <- iso8601DatetimeWithTimezone
  let sql = "\
        \update run \
        \set time_stopped = ? \
        \where run_id = ?"
  1 <- execute conn sql (current8601, runId)
  return ()

instance ToField EnableStatus where
  toField Enabled = toField True
  toField Disabled = toField False

storeGeneration :: Connection -> (Genome -> OriginalFitness) -> UUID -> RunId -> GenerationSn -> Generation -> IO ()
storeGeneration conn fitness generationId runId (GenerationSn sn) (Generation generation) = do
  -- Write the generation info
  let sql = "\
        \insert into generation \
        \(generation_id, run_id, sn) \
        \values (?, ?, ?)"
  1 <- execute conn sql (generationId, runId, sn)

  -- Generate the genome ids, whose sole purpose is to be the key in the database
  -- m (Vector (Vector UUID))
  genomeIds <- flip Vector.mapM generation (\(Species species) ->
    -- m (Vector UUID)
    flip Vector.mapM species (const Random.newGUID))

  -- Write the genome, along with the nodes and the edges
  let genomeSql = "\
        \insert into genome \
        \(genome_id, generation_id, original_fitness, species_sn) \
        \values (?, ?, ?, ?)"
  let nodeSql = "\
        \insert into node \
        \(node_id, genome_id) \
        \values (?, ?)"
  let edgeSql = "\
        \insert into edge \
        \(in_node_id, out_node_id, enabled, weight) \
        \values (?, ?, ?, ?)"
  let (genomeRows, nodeRows'', edgeRows'') = unzip3 . concat . Vector.toList $
        -- Generation is made of species
        -- Vector [(x, [y], [z])]
        flip Vector.imap (Vector.zip genomeIds generation)
          (\speciesSn (speciesGenomeIds, Species species) ->
            -- Species is made of genomes
            -- [(x, [y], [z])]
            Vector.toList $ flip Vector.map (Vector.zip speciesGenomeIds species)
              (\(genomeId, genome@Genome{nodes, edges}) ->
                let genomeRow = (genomeId, generationId, originalFitness genome, speciesSn)
                    nodeRows' = flip map (Map.keys nodes) (\nodeId -> (nodeId, genomeId))
                    edgeRows' = Vector.toList $ Vector.map edgeToRow edges
                in (genomeRow, nodeRows', edgeRows')))
      nodeRows = concat nodeRows''
      edgeRows = concat edgeRows''
  -- TODO @incomplete: Execute this by chunk? (to avoid memory overflow)
  _ <- executeMany conn genomeSql genomeRows
  _ <- executeMany conn nodeSql nodeRows
  _ <- executeMany conn edgeSql edgeRows

  return ()

  where
    originalFitness genome =
      let OriginalFitness fit = fitness genome
      in fit
    edgeToRow Edge{inNodeId, outNodeId, enableStatus, weight} =
      (inNodeId, outNodeId, enableStatus, weight)
