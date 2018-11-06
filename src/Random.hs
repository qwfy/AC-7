module Random
  ( P(..)
  , trigger
  , triggers
  )
where


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
