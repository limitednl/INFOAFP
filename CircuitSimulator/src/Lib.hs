{-# LANGUAGE DataKinds, TypeFamilies, GADTs #-}

module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Nat = Zero
         | Succ Nat

class Comp c where
    getResistance :: c -> Float

newtype Chain = Chain (Float -> Float -> Float)

data SPGraph a where
    Link      :: Chain -> SPGraph a -> SPGraph a -> SPGraph a
    -- Parallel  :: SPGraph a -> SPGraph a -> SPGraph a
    -- Transistor:: Float -> SPGraph a -> SPGraph a -> SPGraph a
    Primitive :: Comp c => c -> SPGraph a

newtype Resistance = Resistance {r :: Float}

data Battery = Battery

instance Comp Resistance where
    getResistance = r

-- instance Comp Battery where
--     type (Input Battery)  = 'Zero
--     type (Output Battery) = 'Zero

calcRes :: SPGraph a -> Float
calcRes (Primitive r0)    = getResistance r0
calcRes (Link (Chain f) r1 r2)    = f (calcRes r1) (calcRes r2)

res0 :: SPGraph a
res0 = Primitive Resistance{r = 5}

res1 :: SPGraph a
res1 = Primitive Resistance{r = 2}

res2 :: SPGraph a
res2 = Primitive Resistance{r = 10}

chain :: SPGraph a
chain = Serial res0 (Parallel res1 res2)