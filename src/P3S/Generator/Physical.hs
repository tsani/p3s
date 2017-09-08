module P3S.Generator.Physical where

import P3S.Generator.Types
import P3S.Physics
import P3S.Position

-- | A physical generator measures pitch in hertz and time in seconds.
-- Its output is an amplitude measurement.
type PhysicalGenerator = Generator Hz S Amplitude

square :: PhysicalGenerator
square = pulse 0.5

-- | Constructs a pulse wave with the given duty cycle between @0@ and @1@,
-- representing the proportion of time spent in the high state.
--
-- Calls @error@ on out-of-bounds duty cycle values.
pulse :: Double -> PhysicalGenerator
pulse d
  | 0 > d = error "duty cycle less than zero"
  | 1 < d = error "duty cycle greater than one"
  | otherwise = Generator $ \f -> Position $ \t -> A . g $ pprop f t where
    g x
      | 0 <= x && x < d = -1
      | d <= x && x < 1 = 1

sinGen :: PhysicalGenerator
sinGen = Generator $ \f -> Position $ \t -> A . g $ pprop f t where
  g x = sin (2 * pi * x)

-- | Computes the position of the time through the period, from 0 to 1.
-- This function is typically used in the construction of physical generators
-- that repeat forever.
pprop :: Hz -> S -> Double
pprop f (S t) = fromIntegral (t `mod` p) / fromIntegral p where
  (S p) = period f
