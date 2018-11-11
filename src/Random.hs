module Random
  ( P(..)
  , trigger
  , triggers
  , choose
  )
where


import Data.List
import qualified System.Random
import Control.Monad

-- | Probability
newtype P = P Float

-- | With probability @triggerP@ being @True@.
trigger :: P -> IO Bool
trigger (P triggerP) = do
  p <- System.Random.randomRIO (0.0, 1.0)
  if p <= triggerP
    then return True
    else return False

triggers :: Int -> P -> IO [Bool]
triggers n p =
  replicateM n (trigger p)

choose :: Foldable t => t (a, Float) -> IO (a, Float)
choose dist
  | null dist = error "Cannot choose from nothing"
  | otherwise = do
    let (accum', total) = foldl' (\(accL, accW) (a, w) ->
          ((a, accW+w) : accL, accW+w)
          ) ([], 0) dist
    let accum = reverse accum'
    p <- System.Random.randomRIO (0.0, total)
    return . head $ dropWhile (\(_, q) -> q < p) accum
