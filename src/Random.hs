module Random
  ( P(..)
  , trigger
  )
where


import qualified System.Random

-- | Probability
newtype P = P Float

-- | With probability @triggerP@ being @True@.
trigger :: P -> IO Bool
trigger (P triggerP) = do
  p <- System.Random.randomRIO (0.0, 1.0)
  if p <= triggerP
    then return True
    else return False
