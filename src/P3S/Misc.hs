module P3S.Misc
( (<#>)
, ($>)
, (<>)
, invariant
) where

import Data.Monoid ( (<>) )

(<#>) :: Functor f => f a -> (a -> b) -> f b
(<#>) = flip (<$>)

($>) :: Functor f => f a -> b -> f b
($>) = flip (<$)

infixl 4 <#>, $>

-- | Constructs an empty functor using the monoid instance, and then fills it
-- with the given value.
invariant :: forall f a. (Monoid (f a), Functor f) => a -> f a
invariant x = mempty @(f a) $> x
