module P3S.Generator.Sample where

import P3S.Apply
import P3S.Physics
import P3S.Generator.Types

-- | Computes a given number of samples from a generator.
sample
  :: forall n t a. Monoid t
  => t -- ^ Offset
  -> t -- ^ Sample width
  -> n -- ^ Note
  -> Generator n t a
  -> Int -- ^ Sample count
  -> [a]
sample off w f (Generator g) n = go n off where
  go :: Int -> t -> [a]
  go 0 _ = []
  go n t = g f @@ t : go (n - 1) t' where
    t' = t <> w

sampleFor
  :: S -- ^ Offset
  -> S -- ^ Sample width
  -> Hz -- ^ Note
  -> Generator Hz S a
  -> S -- ^ Duration
  -> [a]
sampleFor off (S w) f g (S d) = sample off (S w) f g (d `div` w)

