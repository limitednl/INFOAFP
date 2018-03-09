{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

import Nat

class PrimComp c where
    type Input c  :: Nat
    type Output c :: Nat

data SPG (i :: Nat) (o :: Nat) where
    Serial    :: SPG i x -> SPG x o -> SPG i o
    Primitive :: PrimComp c => c -> SPG (Input c) (Output c)
    Par       :: SPG i o1 -> SPG i o2 -> SPG i (Sum o1 o2)

newtype Resistor = Resistor {resistance :: Float}

instance PrimComp Resistor where
    type (Input Resistor)  = Succ Zero
    type (Output Resistor) = Succ Zero

data Battery = Battery {voltage :: Float, amperage :: Float}

instance PrimComp Battery where
    type (Input Battery)  = Succ Zero
    type (Output Battery) = Succ Zero

test = Serial (Primitive (Battery 9 0.2)) (Primitive (Resistor 100))

test2 = Par (Primitive (Resistor 100)) (Serial (Primitive (Resistor 100)) (Primitive (Resistor 100)))