{-|
This is an implementation of
/Evolving Neural Networks Through Augmenting Topologies (2002)/ by
/Kenneth O. Stanley/ and /Risto Miikkulainen/, <http://nn.cs.utexas.edu/?stanley:ec02>.
-}

module Main where

import qualified Data.List

import qualified NEAT.Data
import qualified NEAT.Algo
import qualified NEAT.XOR as Problem

main :: IO ()
main = do
  putStrLn . (Data.List.intercalate " ") $
    ["solving problem:", (NEAT.Data.name Problem.config), "using NEAT"]
  ginVar <- NEAT.Algo.makeGINTVar (NEAT.Data.GIN 0)
  initPopulation <- NEAT.Algo.makeInitPopulation Problem.config ginVar
  NEAT.Algo.simulate initPopulation Problem.fitness Problem.config
