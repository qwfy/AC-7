{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module NEAT.Vis
  ( genomeToDot
  )
  where


import Data.GraphViz
import qualified Data.GraphViz.Types.Generalised as G
import Data.GraphViz.Types.Monadic
import Data.String

import NEAT.Data

genomeToDot :: Genome -> DotGraph String
genomeToDot Genome{edges} = G.fromGeneralised $
  digraph (Str "test") (mapM_ makeEdge edges)
  where
    makeEdge Edge{inNodeId, outNodeId} =
      fromString (show inNodeId) --> fromString (show outNodeId)
