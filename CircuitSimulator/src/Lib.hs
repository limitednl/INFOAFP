{-# LANGUAGE GADTs #-}
module Lib
    ( someFunc
    ) where

import Data.String

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Component = Component {unid       :: String,
                            resistance :: Float,
                            voltage    :: Float,
                            current    :: Float}
data Result = Result {      rOver      :: Float,
                            vOver      :: Float,
                            cOver      :: Float}
    deriving Show

data Link = Link {from :: String, to :: String}
    deriving Show

defaultComponent :: Component
defaultComponent = Component {unid       = "", 
                              resistance = 0,
                              voltage    = 0,
                              current    = 0}
defaultResult :: Result
defaultResult = Result {rOver      = 0,
                        vOver      = 0,
                        cOver      = 0}

components :: [Component]
components = [defaultComponent {unid = "b1", voltage = 2, vOver = 2},
              defaultComponent {unid = "r1", resistance = 5}]

links :: [Link]
links = [Link {from = "b1", to = "r1"}, Link {from = "r1", to = "b1"}]

data SPgraph a = Parallel (SPgraph a) (SPgraph a)
               | Series (SPgraph a) (SPgraph a)
               | Edge a
    deriving Show

testSP :: SPgraph Component
testSP = Series (Edge (head components)) (Edge (components !! 1))

mkrOver :: SPgraph Component -> SPgraph Result