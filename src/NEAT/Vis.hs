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

genomeToDot :: Genome -> Float -> DotGraph String
genomeToDot Genome{edges} fitness = G.fromGeneralised $
  digraph (Str "genome") $ do
    -- TODO @incomplete: hide the label properly
    graphAttrs [toLabel . show $ fitness]
    nodeAttrs [toLabel (" " :: String)]
    mapM_ makeEdge edges
  where
    makeEdge Edge{inNodeId, outNodeId, weight, enableStatus} = do
      let a = show inNodeId
      let b = show outNodeId
      let c = case enableStatus of
                Enabled -> Black
                Disabled -> Gray
      edge a b [toLabel $ show weight, color c, fontColor c]
