module Random
  ( randomBool
  )
where


import qualified System.Random

-- | Generate a random 'Bool' with the 'trueProb probability being 'True'.
randomBool :: Float -> IO Bool
randomBool trueProb = do
  p <- System.Random.randomRIO (0.0, 1.0)
  if p <= trueProb
    then return True
    else return False
