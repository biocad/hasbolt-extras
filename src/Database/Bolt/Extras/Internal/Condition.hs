{-# LANGUAGE ExistentialQuantification #-}

module Database.Bolt.Extras.Internal.Condition
  (
    Condition (..)
  , tautology
  , matches
  , itself
  ) where

-- | Conditional expressions over type 'a' and its mappings.
-- Supported operations:
-- * equality check :==
-- * disunction     :&&
-- * conjunction    :||
--
-- Typical usage:
-- Say we have variable 'var :: a', a function 'f :: a -> b' and a value 'val :: b'.
-- Expression 'f :== b' acts as 'f a == b'
-- Examples:
--
-- > data D = D { fld1 :: Int
-- >            , fld2 :: String
-- >            , fld3 :: Double
-- >            }
-- >
-- > d = D 42 "noononno" 1.618
-- > d `matches` (fld1 :== 12 :&& fld2 :== "abc")
-- > False
-- >
-- > d `matches` (fld1 :== 42 :|| fld3 == 1.0)
-- > True
--
infix  4 :==
infixr 3 :&&
infixr 2 :||
data Condition a = forall b. Eq b => (a -> b) :== b
                 | Condition a :&& Condition a
                 | Condition a :|| Condition a


-- | Check whether data satisfies conditions on it.
--
matches :: a -> Condition a -> Bool
matches obj (transform :== ref) = transform obj == ref
matches obj (u :&& v)           = matches obj u && matches obj v
matches obj (u :|| v)           = matches obj u || matches obj v


-- | Matching 'tautology' will always succeed.
-- > whatever `matches` tautology == True
-- > -- Match is lazy:
-- > undefined `matches` tautology == True
--
tautology :: Condition a
tautology = const True :== True


-- | Object itself instead of its mappings is matched with help of this alias.
-- > 42 `matches` (itself :== 42) == True
-- > 42 `matches` (itself :== 41) == False
--
itself :: a -> a
itself = id
