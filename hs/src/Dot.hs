module Dot
  ( toSvg
  )
  where

import Data.GraphViz
import qualified Data.ByteString as SB

toSvg :: DotGraph String -> IO SB.ByteString
toSvg dot =
  graphvizWithHandle Dot dot Svg SB.hGetContents
