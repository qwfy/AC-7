module NEAT.XOR
  ( fitness
  , params
  )
where

import NEAT.Data
import qualified NEAT.Algo

params :: Params
params = Params
  { name = "XOR"
  , initPopulation = 150
  , guessedGenerations = 50
  , weightRange = (-1.0, 1.0)
  , compatibilityParams = CompatibilityParams
      { c123=(1.0, 1.0, 0.4)
      , threshold=3.0}
  , inNodes = 2
  , outNodes = 1}

-- TODO @incomplete: finish this
fitness :: Genome -> Float
fitness genome =
  (4 - sum losses) ^ 2
  where
    sensorValues = [[0, 0], [0, 1], [1, 0], [1, 1]]
    truths       = [0,      1,      1,      0     ]
    -- each node should have at least one output, so the index by 0 is safe
    predicions = map (!! 0) $ map (\sv -> NEAT.Algo.genomeValue genome sv) sensorValues
    losses = map (\(a, b) -> abs (a - b)) $ zip truths predicions
