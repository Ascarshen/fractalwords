module Types where

import Data.Foldable (toList)
import Linear (M33)

-- rule, node and link ids
type RID = Int
type NID = Int 
type LID = (NID, NID)

swapPair :: (a, b) -> (b, a)
swapPair (a, b) = (b, a)

sortPair :: Ord a => (a, a) -> (a, a)
sortPair ab@(a, b) = if a <= b then ab else (b, a)

uncurry3 :: (a -> b -> c -> r) -> (a, b, c) -> r
uncurry3 f (a, b, c) = f a b c

toLists :: M33 a -> [[a]]
toLists = map toList . toList
