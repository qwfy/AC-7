module Random
  ( P(..)
  , trigger
  , triggers
  , choose
  , chooseWith
  , chooseUniformly
  , newGUID
  )
where


import Data.List
import qualified System.Random
import Control.Monad
import Data.UUID (UUID)
import qualified Data.UUID.V4

newGUID :: IO UUID
newGUID = Data.UUID.V4.nextRandom

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

choose :: Foldable t => t (a, Float) -> IO (Maybe (a, Float))
choose = chooseWith snd

chooseUniformly :: Foldable t => t a -> IO (Maybe a)
chooseUniformly = chooseWith (const 1.0)

-- TODO @incomplete: test this
chooseWith :: Foldable t => (a -> Float) -> t a -> IO (Maybe a)
chooseWith getWeight dist
  | null dist = return Nothing
  | otherwise = do
    let (accum', total) = foldl' (\(accL, accW) a ->
          let w = getWeight a
          in ((a, accW+w) : accL, accW+w)
          ) ([], 0) dist
    let accum = reverse accum'
    p <- System.Random.randomRIO (0.0, total)
    let (chosen, _) = head $ dropWhile (\(_, q) -> q < p) accum
    return $ Just chosen
