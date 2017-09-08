module P3S.Apply where

-- | Class of things that behave like functions.
class Apply f i where
  -- | Result of the application of the function @f@ to the input @i@.
  type Result f i

  -- | Applies a value of type @i@ to the function of type @f@.
  apply :: f -> i -> Result f i

(@@) :: Apply f i => f -> i -> Result f i
(@@) = apply
infixl 7 @@
