module NEAT.XOR
  ( name
  , fitness
  , guessedGenerations
  , guessedInitPopulation
  )
where

import qualified NEAT.Data

name :: String
name = "XOR"

guessedGenerations :: Int
guessedGenerations = 100

guessedInitPopulation :: Int
guessedInitPopulation = 100

fitness :: NEAT.Data.Genome -> Float
fitness = undefined
