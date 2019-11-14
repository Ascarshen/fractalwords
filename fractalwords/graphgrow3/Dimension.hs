-- |
-- Compute Hausdorff dimension of a graph directed iterated function system of
-- similarities specified by adjacency matrix, assuming the open set condition
-- is satisfied.  For example:
--
-- > -- Koch snowflake
-- > > dimH (listArray ((0,0),(0,0)) [[1/3,1/3,1/3,1/3]])
-- > Just 1.2618595361709595
--
-- > -- Sierpinski triangle
-- > > dimH (listArray ((0,0),(0,0)) [[1/2,1/2,1/2]])
-- > Just 1.5849626064300537
--
-- > -- Koch + Sierpinski hybrid
-- > > dimH (listArray ((0,0),(1,1)) [[1/3,1/3,1/3,1/3],[1/2],[1/3],[1/2,1/2,1/2]])
-- > Just 1.7465347051620483

module Dimension (dimH) where

import Data.Array (Array, (!), (//), bounds, indices, listArray)

import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Query.DFS (scc)
import Data.Graph.Inductive.Graph (mkGraph, subgraph, noNodes, labNodes, labEdges)

-- compute dimension as maximum over components
dimH :: Array (Int, Int) [Double] -> Maybe Double
dimH = maximum . (Nothing:) . map dimSCC . components

-- precondition: m is square
components :: Array (Int, Int) [Double] -> [Array (Int, Int) [Double]]
components m = map fromGraph $ map (`subgraph` g) (scc g)
  where
    g = toGraph m

toGraph :: Array (Int, Int) [Double] -> Gr Int [Double]
toGraph m = mkGraph
    [(i, i) | i <- [0..n]]
    [ (i, j, m ! (i, j)) | (i, j) <- indices m, not (null (m ! (i, j))) ]
  where
    ((0, 0), (n, _n)) = bounds m

fromGraph :: Gr Int [Double] -> Array (Int, Int) [Double]
fromGraph g = listArray ((0, 0), (n, n)) (replicate ((n + 1) * (n + 1)) []) //
    [ ((i',j'), l)
    | (i, j, l) <- es, Just i' <- [lookup i ns], Just j' <- [lookup j ns]
    ]
  where
    n = noNodes g - 1
    ns = map fst (labNodes g) `zip` [0..]
    es = labEdges g

-- precondition: g corresponds to a strongly connected component
dimSCC :: Array (Int, Int) [Double] -> Maybe Double
dimSCC g = findZero f 0 10
  where
   f s = fmap (subtract 1) $ spectralRadius (fmap (sum . map (** s)) g)

-- precondition: m is square
spectralRadius :: Array (Int, Int) Double -> Maybe Double
spectralRadius m = go 0 1 0 ones
  where
    ((0, 0), (m1,_m1)) = bounds m
    ones = listArray (0, m1) (replicate (m1 + 1) 1)
    go :: Int -> Double -> Double -> Array Int Double -> Maybe Double
    go n h r v
      | n > 1000 = Nothing
      | abs h < 0.00001 = Just r
      | otherwise = go (n + 1) h' r' v'
      where
        h' = r' - r
        r' = dot v' v / dot v v
        v' = mul m v

-- precondition: a and b have the same bounds
dot :: Array Int Double -> Array Int Double -> Double
dot a b = sum [ a ! i * b ! i | i <- indices a ]

-- precondition: m is square matching the dimensions of v
mul :: Array (Int, Int) Double -> Array Int Double -> Array Int Double
mul m v = listArray (bounds v)
  [ sum [ m ! (i, j) * v ! j | j <- indices v ] | i <- indices v ]

-- precondition: f is continuous and monotonic with a zero between x1 and x2
findZero :: (Double -> Maybe Double) -> Double -> Double -> Maybe Double
findZero f x1' x2' = do
  y1 <- f x1'
  y2 <- f x2'
  case () of
    _ | zero y1 -> Just x1'
      | zero y2 -> Just x2'
      | y1 > 0 && y2 > 0 -> Nothing
      | y1 < 0 && y2 < 0 -> Nothing
      | otherwise -> go x1' y1 x2' y2
  where
    zero x = abs x < 0.0000001
    go x1 y1 x2 y2 = do
      let x = (x1 + x2) / 2
      y <- f x
      case () of
        _ | zero y -> Just x
          | (y2 > 0 && y > 0) || (y2 < 0 && y < 0) -> go x1 y1 x y
          | (y2 > 0 && y < 0) || (y2 < 0 && y > 0) -> go x y x2 y2
          | otherwise -> Nothing
