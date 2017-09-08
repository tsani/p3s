module P3S.Position
( Position(..)
, cutoff
, cutoffWith
, offset
, offsetWith
) where

import P3S.Apply
import P3S.Profunctor
import P3S.Math
import P3S.Misc

-- | A time-varying quantity.
newtype Position t a = Position { timed :: t -> a }

-- | Introduce a cutoff point to a time curve.
-- The monoid instance is used to produce the value beyond the cutoff.
cutoff :: (Ord t, Monoid a) => t -> Position t a -> Position t a
cutoff = cutoffWith mempty

-- | Introduce a cutoff point to a time curve.
-- The given value is used after the cutoff.
cutoffWith :: Ord t => a -> t -> Position t a -> Position t a
cutoffWith x c p = Position go where
  go t
    | c < t = x
    | c >= t = p @@ t
    | otherwise = error "bullshit Ord instance"

-- | Introduce an offset to the front of a time curve.
-- The group (monoid) instance is used to produce the value to use before the
-- cutoff.
--
-- In contrast with 'cutoff', a 'Group' instance is required on the time,
-- because we need to compute a difference.
offset :: (Ord t, Group t, Monoid a) => t -> Position t a -> Position t a
offset = offsetWith mempty

-- | Introduce an offset to the front of a time curve.
-- The given value is used before the offset.
--
-- In contrast with 'cutoff', a 'Group' instance is required on the time,
-- because we need to compute a difference.
offsetWith :: (Ord t, Group t) => a -> t -> Position t a -> Position t a
offsetWith x c p = Position go where
  go t
    | t < c = x
    | t >= c = p @@ (t `diff` c)
    | otherwise = error "bullshit Ord instance"

----- INSTANCES ---------------------------------------------------------------

instance Profunctor Position where
  lmap f (Position p) = Position (p . f)
  rmap g (Position p) = Position (g . p)
  dimap f g (Position p) = Position (g . p . f)

instance Functor (Position t) where
  fmap = rmap

instance Applicative (Position t) where
  pure = Position . const
  p <*> x = Position $ \t -> (p @@ t) (x @@ t)

instance Monad (Position t) where
  return = pure
  p >>= k = Position $ \t -> k (p @@ t) @@ t

instance Apply (Position t a) t where
  type Result (Position t a) t = a
  apply (Position f) t = f t

instance Monoid a => Monoid (Position t a) where
  mempty = invariant mempty
  mappend (Position t1) (Position t2) = Position $ \t -> t1 t <> t2 t
