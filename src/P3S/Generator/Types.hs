module P3S.Generator.Types
( Generator(..)
) where

import P3S.Apply
import P3S.Misc
import P3S.Position
import P3S.Profunctor

-- | A generator is a function that represents a sound.
--
-- Generators can be stacked using their monoid instance, provided that their
-- output be itself a monoid.
--
-- Generators can be combined pointwise using their applicative instance.
newtype Generator n t a = Generator (n -> Position t a)

----- INSTANCES ---------------------------------------------------------------

instance Functor (Generator n t) where
  fmap f (Generator g) =
    Generator $ \freq -> Position $ \t -> f $ g freq @@ t

instance Applicative (Generator n t) where
  pure = Generator . const . Position . const
  (<*>) g x = Generator $ \n -> Position $ \t -> (g @@ n @@ t) (x @@ n @@ t)

instance Monad (Generator n t) where
  return = pure
  (>>=) g m = Generator $ \n -> Position $ \t -> m (g @@ n @@ t) @@ n @@ t

instance Apply (Generator n t a) n where
  type Result (Generator n t a) n = Position t a
  apply (Generator g) n = g n

instance Monoid a => Monoid (Generator n t a) where
  mempty = invariant mempty
  mappend (Generator g1) (Generator g2) = Generator $ \n -> g1 n <> g2 n

instance Profunctor (Generator n) where
  lmap f (Generator g) = Generator (lmap f . g)
  rmap f (Generator g) = Generator (rmap f . g)
  dimap l r (Generator g) = Generator (dimap l r . g)

-- -- | Extracts the time type parameter.
-- type family Time (g :: *) :: * where
--   Time (Position t a) = t
--   Time (Generator n t a) = t

-- -- | A timable value with an attached time.
-- data Timed (g :: *) :: * where
--   Timed :: !(Time g) -> !g -> Timed g

-- instance Apply f i => Apply (Timed f) i where
--   type Result (Timed f) i = Result f i
--   apply (Timed _ f) x = f @@ x

-- -- | Timed generators can be combined sequentially by their monoid instance,
-- -- provided that time values, of type @t@, form a totally ordered group
-- -- and that output values, of type @a@, form a monoid.
-- instance (Group t, Monoid a, Ord t) => Monoid (Timed (Generator n t a)) where
--   mempty = Timed mempty mempty
--   mappend (Timed t1 g1) (Timed t2 g2) = Timed (t1 <> t2) (Generator go) where
--     go :: n -> Position t a
--     go n = Position go' where
--       go' :: t -> a
--       go' t
--         | t < mempty = mempty
--         | mempty <= t && t < t1 = g1 @@ n @@ t
--         | t1 <= t && t < t2 = g2 @@ n @@ (t `diff` t1)
--         | t2 <= t = mempty
--         | otherwise = error "does time even exist?"
