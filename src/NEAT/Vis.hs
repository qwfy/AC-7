{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module NEAT.Vis
  ( genomeToDot
  )
  where


import Data.GraphViz
import qualified Data.GraphViz.Types.Generalised as G
import Data.GraphViz.Types.Monadic

import NEAT.Data

genomeToDot :: Genome -> DotGraph String
genomeToDot Genome{edges} = G.fromGeneralised $
  digraph (Str "genome") $ do
    -- TODO @incomplete: hide the label properly
    nodeAttrs [toLabel (" " :: String)]
    mapM_ makeEdge edges
  where
    makeEdge Edge{inNodeId, outNodeId, weight} = do
      let a = show inNodeId
      let b = show outNodeId
      edge a b [toLabel $ show weight]
