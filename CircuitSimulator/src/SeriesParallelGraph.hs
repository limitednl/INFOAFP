module SeriesParallelGraph (SPGraph (..), parallelComp, seriescomp, width, height) where

-- | A Series-Parallel Graph data type.
data SPGraph a = Parallel (SPGraph a) (SPGraph a) -- ^ Connects two SPGraphs parallel
               | Series (SPGraph a) (SPGraph a)   -- ^ Connects two SPGraphs sequentially
               | Edge a                           -- ^ Represents an edge with a value
    deriving Show

-- | Parallel composition of two Series-Parallel Graphs
parallelComp :: SPGraph a -> SPGraph a -> SPGraph a
parallelComp = Parallel

-- | Series composition of two Series-Parallel Graphs
seriesComp :: SPGraph a -> SPGraph a -> SPGraph a
seriesComp = Series

-- | Calculate the maximum number of edges below each other in an SPGraph
height :: SPGraph a -> Int
height (Parallel a b) = max (height a) (height b)
height (Series a b)   = height a + height b
height (Edge _)       = 1

-- | Calculates the maximum number of edges next to each other in an SPGRaph
width :: SPGraph a -> Int
width (Parallel a b) = (width a) + (width b)
width (Series a b)   = max (width a) (width b)
width (Edge _)       = 1