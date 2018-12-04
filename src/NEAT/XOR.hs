module NEAT.XOR
  ( fitness
  , params
  )
where

import Random
import NEAT.Data
import qualified NEAT.Algo

params :: Params
params = Params
  { name = "XOR"
  , numInitPopulation = 150
  , numGuessedGenerations = 50
  , weightRange = (-1.0, 1.0)
  , compatibilityParams = CompatibilityParams
      { c123 = (1.0, 1.0, 0.4)
      , threshold = 3.0}
  , numInNodes = 2
  , numOutNodes = 1
  , mutateParams = MutateParams
      { mutateWeightP   = P 0.8
      , perturbeWeightP = P 0.9
      , perturbeRange   = (-0.1, 0.1) -- TODO @incomplete: confirm this
      , mutateAddNodeP  = P 0.03
      , mutateAddEdgeP  = P 0.05
      , geneDisableP    = P 0.75 }
  }

-- TODO @incomplete: finish this
fitness :: Genome -> OriginalFitness
fitness genome =
  OriginalFitness $ (4 - sum losses) ^ (2 :: Int)
  where
    sensorValues = [[0, 0], [0, 1], [1, 0], [1, 1]]
    truths       = [0,      1,      1,      0     ]
    -- each node should have at least one output, so the index by 0 is safe
    predicions = map (!! 0) $ map (\sv -> NEAT.Algo.genomeValue genome sv) sensorValues
    losses = map (\(a, b) -> abs (a - b)) $ zip truths predicions
