{-# OPTIONS_GHC -Wall #-}

module Challenge where

-- Challenge: can you make an instance of `Monoid` for `Bool`?
-- How many different instances are there?

{- Any: wraps bool so we can create a Bool Monoid instance
   giving or as the associative operator and False as the identity element.
   The name stands for: apply or on a given sequence of booleans
   it's true if at least one of them is true. -}
newtype Any = Any Bool deriving (Show, Eq)

instance Semigroup Any where
    Any a <> Any b = Any $ a || b

instance Monoid Any where
    mempty = Any False

{- All: the same as Any but using and as the associative operator and
   True as the identity element -}
newtype All = All Bool deriving (Show, Eq)

instance Semigroup All where
    All a <> All b = All $ a && b

instance Monoid All where
    mempty = All True