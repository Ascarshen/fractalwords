module Fractal.GraphGrow.Utils
  ( -- * Miscellaneous maths.
    sum'
  , sqr
  , clamp
  , roundUpTwo
    -- * Strict tuples.
  , STwo(..)
  , SThree(..)
    -- * Ordered lists.
  , mergeSortedBy
  , mergeSortedsBy
  ) where

import Data.List (foldl')

sum' :: Num a => [a] -> a
sum' = foldl' (+) 0

sqr :: Num a => a -> a
sqr a = a * a

clamp :: Ord a => a -> a -> a -> a
clamp x mi ma = mi `max` x `min` ma

roundUpTwo :: (Num a, Ord a) => a -> a
roundUpTwo n = head . dropWhile (< n) . iterate (2 *) $ 1

data STwo a b = STwo{ fstTwo :: !a, sndTwo :: !b }
  deriving (Read, Show, Eq, Ord)

data SThree a b c = SThree{ fstThree :: !a, sndThree :: !b, thdThree :: !c }
  deriving (Read, Show, Eq, Ord)

mergeSortedBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeSortedBy cmp = go
  where
    go [] ys = ys
    go xs [] = xs
    go xxs@(x:xs) yys@(y:ys) = case x `cmp` y of
      LT -> x : go xs yys
      EQ -> x : y : go xs ys
      GT -> y : go xxs ys

mergeSortedsBy :: (a -> a -> Ordering) -> [[a]] -> [a]
mergeSortedsBy cmp = foldr (mergeSortedBy cmp) []
