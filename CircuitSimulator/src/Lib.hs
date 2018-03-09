{-# LANGUAGE DataKinds, TypeFamilies, GADTs #-}

module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Nat = Zero
         | Succ Nat

class Comp c where
    type Input c  :: Nat
    type Output c :: Nat
    getResistance :: c -> Float

data SPGraph (i :: Nat) (o :: Nat) where
    Serial    :: SPGraph i o -> SPGraph i o -> SPGraph i o
    Parallel  :: SPGraph i o -> SPGraph i o -> SPGraph i o
    Primitive :: Comp c => c -> SPGraph (Input c) (Output c)

newtype Resistance = Resistance {r :: Float}

data Battery = Battery

instance Comp Resistance where
    type (Input Resistance)  = 'Zero
    type (Output Resistance) = 'Zero
    getResistance = r

-- instance Comp Battery where
--     type (Input Battery)  = 'Zero
--     type (Output Battery) = 'Zero

calcRes :: SPGraph (Input Resistance) (Output Resistance) -> Float
calcRes (Primitive r0)    = getResistance r0
calcRes (Serial r1 r2)    = calcRes r1 + calcRes r2
calcRes (Parallel r1 r2)  = 1 / ((1 / calcRes r1) + (1 / calcRes r2))

res0 :: SPGraph (Input Resistance) (Output Resistance)
res0 = Primitive Resistance{r = 5}

res1 :: SPGraph (Input Resistance) (Output Resistance)
res1 = Primitive Resistance{r = 2}

res2 :: SPGraph (Input Resistance) (Output Resistance)
res2 = Primitive Resistance{r = 10}

chain :: SPGraph (Input Resistance) (Output Resistance)
chain = Serial res0 (Parallel res1 res2)