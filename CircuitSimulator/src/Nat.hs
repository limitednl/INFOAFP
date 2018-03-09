-- | A module for natural numbers
module Nat (Nat(..)) where

-- | A datatype representing natural numbers.
--   for example, 5 would be Succ (Succ (Succ (Succ (Succ Zero))))
--   or Sum (Succ (Succ Zero)) (Succ (Succ (Succ Zero))) or any
--   other sum resulting in 5.
data Nat = Zero         -- ^ Represents 0
         | Succ Nat     -- ^ Succ x represents (x + 1)
         | Sum Nat Nat  -- ^ Sum a b represents (a + b)
    deriving (Eq)

-- | Converts a natural number to a regular integer
-- >>> toInt (Succ (Succ (Succ Zero)))
-- 3
-- >>> toInt (Sum (Succ Zero) (Succ (Succ Zero)))
-- 3 
toInt :: Nat -> Integer
toInt Zero        = 0
toInt (Succ x)    = toInt x + 1
toInt s@(Sum x y) = toInt x + toInt y

-- | Converts a regular integer to a natural number, if possible.
-- >>> fromInt 3
--     Succ (Succ (Succ Zero))
-- >>> fromInt (-3)
-- *** Exception ***
fromInt :: Integer -> Nat
fromInt x |  x < 0    = error "Negative integers are not natural numbers"
          | x == 0    = Zero
          | otherwise = Succ $ fromInt (x-1)

-- | Removes all Sums from a natural number and simplifies them
-- to Succs and Zeros
foldSum :: Nat -> Nat
foldSum (Sum a b) = fromInt $ toInt (foldSum a) + toInt (foldSum b)
foldSum Zero      = Zero
foldSum (Succ x)  = Succ $ foldSum x

-- | Represents natural numbers as normal integers
instance Show Nat where
    show = show . toInt

instance Num Nat where
    (+) a Zero            = a
    (+) Zero a            = a
    (+) a (Succ b)        = Succ a + b
    (+) a (Sum b c)       = a + b + c
    (*) _ Zero            = Zero
    (*) Zero _            = Zero
    (*) a b               = fromInt $ toInt a * toInt b
    (-) a Zero            = a
    (-) Zero a            = error "Can't subtract from zero"
    (-) (Succ a) (Succ b) = a - b
    (-) a@(Sum _ _) b     = foldSum a - b
    (-) a b@(Sum _ _)     = a - foldSum b
    abs                   = id
    signum _              = Succ Zero
    fromInteger           = fromInt
