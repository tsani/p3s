module P3S.Physics
( Hz(..)
, period
, S(..)
, sec
, ms
, us
, ns
, Amplitude(..)
, Intensity(..)
, invert
, unsafeIntensity
, (<>)
, scale
, Group(..)
) where

import P3S.Math ( Group(..), Field(..), Ring(..), Between(..) )

import Data.Function ( on )
import Data.Monoid ( (<>) )

newtype Hz = Hz { hz :: Int }
  deriving (Eq, Ord, Show)

-- | Invert a frequency to obtain a period.
period :: Hz -> S
period (Hz f) = S (1000000000 `div` f)

-- | A length of time, measured in seconds.
newtype S = S { nanoseconds :: Int }
  deriving (Eq, Ord, Show)

-- | Specify a length of time in seconds.
sec :: Int -> S
sec = S . (*) 1000000000

-- | Specify a length of time in milliseconds.
ms :: Int -> S
ms = S . (*) 1000000

-- | Specify a length of time in microseconds.
us :: Int -> S
us = S . (*) 1000

-- | Specify a length of time in nanoseconds.
ns :: Int -> S
ns = S

-- | An amplitude is a double-precision floating point number between -1 and 1.
newtype Amplitude = A { amplitude :: Double }

-- | The zero amplitude.
silence :: Amplitude
silence = A 0

-- | An intensity is a measure from 0 to 1 of how strong something is.
newtype Intensity = Intensity Double

-- | Flip an intensity.
invert :: Intensity -> Intensity
invert (Intensity i) = Intensity (1 - i)

scale :: Intensity -> Amplitude -> Amplitude
scale (Intensity i) (A a) = A (i * a)

unsafeIntensity :: Double -> Intensity
unsafeIntensity v
  | 0 > v || v > 1 = error "intensity out of bounds"
  | otherwise = Intensity v

----- INSTANCES ---------------------------------------------------------------

instance Bounded Intensity where
  maxBound = Intensity 1
  minBound = Intensity 0

-- | Intensities are combined with multiplication, but the identity element is
-- zero!
instance Monoid Intensity where
  mempty = Intensity 0
  mappend (Intensity x) (Intensity y) = Intensity (x * y)

instance Monoid S where
  mempty = S 0
  mappend (S x) (S y) = S (x + y)

instance Monoid Amplitude where
  mempty = silence
  mappend (A x) (A y) = A (x + y)

instance Group S where
  inv (S t) = S (negate t)
  diff (S t1) (S t2) = S (t1 - t2)

instance Group Amplitude where
  inv (A a) = A (negate a)
  diff (A a1) (A a2) = A (a1 - a2)

instance Ring S where
  rempty = S 1
  mult (S x) (S y) = S (x * y)

instance Between S where
  type Progress S = Intensity
  between (S lo) (S hi) (S x) = Intensity $ (x - lo) // (hi - lo) where
    (//) = (/) `on` fromIntegral
