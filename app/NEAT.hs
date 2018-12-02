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

import Util
import Data.AC7

main :: IO ()
main = do
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
