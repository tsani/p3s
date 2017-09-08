module P3S.Generator
( module P3S.Generator.Types
, constant
, liftPosition
) where

import P3S.Generator.Types
import P3S.Position

-- | A constant generator ignores its input pitch and always produces the same
-- curve.
constant :: Position t a -> Generator n t a
constant = Generator . const

-- | Lift a transformation of positions into a transformation of generators.
liftPosition
  :: (Position t a -> Position s b)
  -> Generator n t a -> Generator n s b
liftPosition f (Generator g) = Generator (f . g)
