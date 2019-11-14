module GraphGrow.Analysis.Statistics where

    import Data.List (foldl')
    import Data.Monoid (Monoid(..))
    
    data Stat s = Stat{ s0, s1, s2 :: !s }
      deriving (Eq, Ord, Read, Show)
    
    instance Num s => Monoid (Stat s) where
      mempty = Stat{ s0 = 0, s1 = 0, s2 = 0 }
      a `mappend` b = Stat{ s0 = s0 a + s0 b, s1 = s1 a + s1 b, s2 = s2 a + s2 b }
      mconcat = foldl' mappend mempty
    
    stat :: Num s => s -> Stat s
    stat s = Stat{ s0 = 1, s1 = s, s2 = s * s }
    
    getCount :: Stat s -> s
    getCount = s0
    
    getSum :: Stat s -> s
    getSum = s1
    
    getMean :: Fractional s => Stat s -> s
    getMean s = s1 s / s0 s
    
    getVariance :: Fractional s => Stat s -> s
    getVariance s = (s0 s * s2 s - s1 s * s1 s) / (s0 s * s0 s)
    
    getStdDev :: Floating s => Stat s -> s
    getStdDev = sqrt . getVariance
    
    type StatOrd s = (Stat s, MinMax s)
    
    statOrd :: Num s => s -> StatOrd s
    statOrd s = (stat s, minMax s)
    
    newtype MinMax s = MinMax{ getMinMax :: Maybe (s, s) }
      deriving (Eq, Ord, Read, Show)
    
    minMax :: s -> MinMax s
    minMax s = MinMax $! Just $! (((,) $! s) $! s)
    
    instance Ord s => Monoid (MinMax s) where
      mempty = MinMax Nothing
      MinMax Nothing `mappend` b = b
      a `mappend` MinMax Nothing = a
      MinMax (Just (ain, aax)) `mappend` MinMax (Just (bin, bax)) = MinMax $! Just $! (((,) $! ain `min` bin) $! aax `max` bax)
      mconcat = foldl' mappend mempty
    