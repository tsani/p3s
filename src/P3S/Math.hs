module P3S.Math
( Group(..)
, Ring(..)
, Field(..)
, Between(..)
, (<>)
) where

import Data.Monoid ( (<>) )

-- | Types for which an inverse that respects the monoid can be computed.
--
-- (Additive) identity law:
-- @inv mempty = mempty@
--
-- (Additive) cancellation law:
-- @forall x: inv x <> x = mempty@
class Monoid a => Group a where
  -- | Computes the inverse of a value.
  inv :: a -> a
  inv x = mempty `diff` x

  -- | @x `diff` y@ computes @x - y@.
  diff :: a -> a -> a
  diff x y = x <> inv y

-- | A ring is an abelian (commutative) group with an additional distinguished
-- element as well as a new operation, satisfying the following laws.
--
-- (Multiplicative) identity law:
-- @forall x. mult rempty x = mult x rempty = rempty@
--
-- (Multiplicative) associativity law:
-- @forall x y z. mult x (mult y z) = mult (mult x y) z@
--
-- (Additive) commutativity law:
-- @forall x y. x <> y = y <> x@
--
-- Right distributivity law:
-- @mult x (y <> z) = mult x y <> mult x z@
--
-- Left distributivity law:
-- @mult (y <> z) x = mult y x <> mult z x@
class Group a => Ring a where
  rempty :: a
  mult :: a -> a -> a

-- | A field is a commutative ring in which all elements (except the additive
-- identity) have multiplicative inverses, satisfying the following law:
--
-- Multiplicative cancellation law:
-- @forall x. rinv x `mult` x = rempty@
class Ring a => Field a where
  rinv :: a -> a

-- | Types that are linearly ordered and that admit a way of measuring how far
-- along we are between two points.
--
-- A common choice for the result type @Progress a@ is @Intensity@ from
-- "P3S.Physics", since it is meant to wrap a @Double@ between zero and one.
class Between a where
  type Progress a

  -- | Measure how far along we are between two points.
  between
    :: a -- ^ lo
    -> a -- ^ hi
    -> a -- ^ point to measure
    -> Progress a

-- | A class much like Enum, but where the tags are /modular/, i.e. they wrap
-- around.
class ModularEnum a where
  modSucc :: a -> a
  modPred :: a -> a

  modEnumFrom :: a -> [a]
  modEnumFrom = iterate modSucc
