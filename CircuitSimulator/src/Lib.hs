{-# LANGUAGE DataKinds, TypeFamilies, TypeInType, GADTs #-}

module Lib where

data Nat = Succ Nat | Zero

class Component c where
    type Resistance c :: Nat
    toNat :: c -> Nat
    
data SPT (r :: Nat) where
    S :: SPT r -> SPT a -> SPT r
    Prim :: (Show c, Component c) => c -> SPT r

data Resistor = Res Nat | Null

instance Component Resistor where
    type Resistance Resistor = 'Zero
    toNat (Res _) = Zero

instance Show Nat where
    show n = show $ asNum n

instance Show Resistor where
    show (Res n) = "Res " ++ show n

instance Show (SPT r) where
    show (Prim r) = "Prim (" ++ show r ++ ")"

asNum :: Nat -> Int
asNum (Succ n) = 1 + asNum n
asNum Zero     = 0

asNat :: Int -> Nat    
asNat i | i > 0     = Succ $ asNat $ i - 1
        | otherwise = Zero    


