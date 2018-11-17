{-|
This is an implementation of
/Evolving Neural Networks Through Augmenting Topologies (2002)/ by
/Kenneth O. Stanley/ and /Risto Miikkulainen/, <http://nn.cs.utexas.edu/?stanley:ec02>.
-}

{-# LANGUAGE QuasiQuotes #-}

module Main where

import qualified Data.List

import NEAT.Data
import qualified NEAT.Algo
import qualified NEAT.XOR as Problem
import qualified Data.Vector as Vector
import Data.Vector (Vector)
import Path
import Path.IO
import Control.Monad

import Util

main :: IO ()
main = do
  runId <- currentTime
  putStrLn . unwords $
    ["starting run:", runId]
  putStrLn . unwords $
    ["solving problem:", name Problem.config]
  ginVar <- NEAT.Algo.makeGINTVar (GIN 0)
  initPopulation <- NEAT.Algo.makeInitPopulation Problem.config ginVar
  let numGenerations = guessedGenerations Problem.config
  visDir <- liftM2 (\a b -> a </> [reldir|visualization|] </> b) getCurrentDir (parseRelDir runId)
  void $ NEAT.Algo.simulate Problem.fitness (compatibilityParams Problem.config) numGenerations initPopulation visDir
