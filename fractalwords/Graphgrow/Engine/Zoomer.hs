module GraphGrow.Engine.Zoomer
  ( ZoomerOptions(..)
  , defaultZoomerOptions
  , Zoomer
  , zoomer
  , Zoomed(..)
  ) where

import Control.Parallel (par)
import Data.List (minimumBy)
import Data.Ord (comparing)
import System.Random (RandomGen, randomR)

import GraphGrow.Engine.Geometry
import GraphGrow.Engine.Graph
import GraphGrow.Engine.Grow
import GraphGrow.Utils

type Zoomer = Double -> Zoomed
data Zoomed = Zoomed{ zoomedPoints :: [STwo Point Double], zoomedZoomer :: Zoomer }

data ZoomerOptions g = ZoomerOptions
  { zoomerOuterBox   :: Box
  , zoomerInnerBox   :: Box
  , zoomerPerturb    :: Double
  , zoomerReCenter   :: Double
  , zoomerZoomFactor :: Double
  , zoomerGraph      :: g
  , zoomerSeed       :: [Context]
  , zoomerCenter     :: Point
  }

zoomer :: (RandomGen r, Graph g) => ZoomerOptions g -> r -> Zoomer
zoomer (ZoomerOptions a b c d e f g h) = zoomer' a b c d e f g h
  where
    zoomer' box cbox perturb recenter zoomFactor graph = go
      where
        go seed centerP rg0 threshold =
          let points = concatMap (map deplus . grow graph box threshold True) seed
              deplus (SThree _ p s) = STwo p s
              (dx, rg1) = randomR (-1, 1) rg0
              (dy, rg2) = randomR (-1, 1) rg1
              dp = Point dx dy
              target = clampBox cbox . addPoint centerP . scalePoint perturb $ dp
              central = comparing (distanceSquared target)
              minimumBy' _ [] = Point 0 0
              minimumBy' cmp xs = minimumBy cmp xs
              small = map fstTwo . filter ((<= threshold) . sndTwo)
              centerP'@(Point x y) = minimumBy' central (small points)
              z = translate (recenter * x) (recenter * y) `o`
                  scale zoomFactor `o`
                  translate (negate x) (negate y)
              seed' = concatMap (renormalize graph box . zoom z zoomFactor) seed
              next = go seed' centerP' rg2
          in  centerP' `par` Zoomed points next

defaultZoomerOptions :: g -> Node -> ZoomerOptions g
defaultZoomerOptions graph node = ZoomerOptions
  { zoomerOuterBox   = box
  , zoomerInnerBox   = scaleBox 0.5 box
  , zoomerPerturb    = 0.002 * w
  , zoomerReCenter   = 0.995
  , zoomerZoomFactor = 10 ** (1 / 60)
  , zoomerGraph      = graph
  , zoomerSeed       = [Context node [] 1 identity]
  , zoomerCenter     = Point 0 0
  }
  where
    box = Box (-w) (-h) w h
    w = aspect * h
    h = 16
    aspect = 16/9
