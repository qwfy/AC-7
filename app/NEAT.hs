{-|
This is an implementation of
/Evolving Neural Networks Through Augmenting Topologies (2002)/ by
/Kenneth O. Stanley/ and /Risto Miikkulainen/, <http://nn.cs.utexas.edu/?stanley:ec02>.
-}

{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import NEAT.Data
import qualified NEAT.Algo
import qualified NEAT.XOR as Problem
import qualified Log as Log
import Control.Monad
import Options.Applicative
import Data.Monoid ((<>))
import qualified GraphDb
import qualified Database.Bolt as Bolt
-- TODO @incomplete: remove this
import qualified NEAT.Store
import qualified NEAT.StorePg

import Util
import Data.AC7

data Option = Simulate | InitDb | ClearDb

optionParser :: Parser Option
optionParser = subparser
  (  command "simulate" (info (pure Simulate) (fullDesc <> progDesc "Run a simulation"))
  <> command "init-db" (info (pure InitDb) (fullDesc <> progDesc "Initialize the database"))
  <> command "clear-db" (info (pure ClearDb) (fullDesc <> progDesc "Clear the database"))
  )

main :: IO ()
main = do
  let optionParser' = info (optionParser <**> helper) (fullDesc <> progDesc "AC-7")
  opt <- execParser optionParser'
  case opt of
    InitDb -> initDb
    Simulate -> simulate
    ClearDb -> NEAT.StorePg.clearDb


initDb :: IO ()
initDb = do
  pipe <- Bolt.connect GraphDb.config
  NEAT.Store.createConstraint pipe


simulate :: IO ()
simulate = do
  runId <- RunId <$> currentTime

  Log.info ["starting run:", show runId]
  Log.info ["solving problem:", name Problem.params]

  ginVar <- NEAT.Algo.makeGINTVar (GIN 0)
  initPopulation <- NEAT.Algo.makeInitPopulation Problem.params ginVar
  void $ NEAT.Algo.simulate
           runId
           Problem.fitness
           (compatibilityParams Problem.params)
           (weightRange Problem.params)
           (mutateParams Problem.params)
           ginVar
           (numGuessedGenerations Problem.params)
           initPopulation
