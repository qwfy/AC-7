module NEAT.XOR
  ( fitness
  , config
  )
where

import qualified NEAT.Data
import qualified NEAT.Algo

config :: NEAT.Data.Config
config = NEAT.Data.Config
  { NEAT.Data.name = "XOR"
  , NEAT.Data.initPopulation = 150
  , NEAT.Data.guessedGenerations = 50
  , NEAT.Data.weightRange = (-1.0, 1.0)
  , NEAT.Data.threshold = 3.0}

-- TODO @incomplete: finish this
fitness :: NEAT.Data.Genome -> Float
fitness genome =
  (4 - sum losses) ^ 2
  where
    sensorValues = [[0, 0], [0, 1], [1, 0], [1, 1]]
    truths       = [0,      1,      1,      0     ]
    -- each node should have at least one output, so the index by 0 is safe
    predicions = map (!! 0) $ map (\sv -> NEAT.Algo.genomeValue genome sv) sensorValues
    losses = map (\(a, b) -> abs (a - b)) $ zip truths predicions
