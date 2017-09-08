module P3S.Contravariant where

class Contravariant f where
  contramap :: (a -> b) -> f b -> f a

  (>$) :: b -> f b -> f a
  (>$) = contramap . const

(>$<) :: Contravariant f => (a -> b) -> f b -> f a
(>$<) = contramap

(>#<) :: Contravariant f => f b -> (a -> b) -> f a
(>#<) = flip contramap

($<) :: Contravariant f => f b -> b -> f a
($<) = flip (>$)

infixl 4 >$, $<, >$<, >#<
