module NEAT.XOR
  ( fitness
  , config
  )
where

import qualified NEAT.Data

config :: NEAT.Data.Config
config = NEAT.Data.Config
  { NEAT.Data.name = "XOR"
  , NEAT.Data.initPopulation = 100
  , NEAT.Data.guessedGenerations = 50
  , NEAT.Data.weightRange = (-1.0, 1.0)
  }

fitness :: NEAT.Data.Genome -> Float
fitness = undefined
