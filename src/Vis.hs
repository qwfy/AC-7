module Vis
  ( writeSvg
  )
  where

import Data.GraphViz

import Path


writeSvg :: DotGraph String -> Path.Path Abs File -> IO (Path.Path Abs File)
writeSvg dot file = do
  filepath <- addExtension (runGraphvizCommand Dot dot) Svg (toFilePath file)
  parseAbsFile filepath
