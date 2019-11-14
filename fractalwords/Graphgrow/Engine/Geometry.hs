module GraphGrow.Engine.Geometry
  ( -- * Affine transforms.
    Transform(Transform)
  , identity
  , o
  , compose
  , scale
  , rotate
  , translate
    -- * Vectors.
  , Point(Point)
  , scalePoint
  , addPoint
  , distanceSquared
    -- * Inspecting transforms.
  , center
  , sizeSquared
    -- * Bounding boxes.
  , Box(Box)
  , makeBox
  , scaleBox
  , boxSize
  , clampBox
  , outsideBox
  , ) where

import Data.List (foldl')
import Foreign (Storable(..), castPtr)

import Fractal.GraphGrow.Utils (sqr, clamp)

data Transform = Transform !Double !Double !Double !Double !Double !Double
  deriving (Read, Show)
instance Storable Transform where
  alignment _ = alignment (0 :: Double)
  sizeOf _ = 6 * sizeOf (0 :: Double)
  poke p (Transform a b c d e f) = do
    let q = castPtr p
    pokeElemOff q 0 a
    pokeElemOff q 1 b
    pokeElemOff q 2 c
    pokeElemOff q 3 d
    pokeElemOff q 4 e
    pokeElemOff q 5 f
  peek p = do
    let q = castPtr p
    a <- peekElemOff q 0
    b <- peekElemOff q 1
    c <- peekElemOff q 2
    d <- peekElemOff q 3
    e <- peekElemOff q 4
    f <- peekElemOff q 5
    return (Transform a b c d e f)

identity :: Transform
identity = Transform 1 0 0 1 0 0

o :: Transform -> Transform -> Transform
Transform a b c d e f `o` Transform u v w x y z = Transform
  (a * u + b * w) (a * v + b * x)
  (c * u + d * w) (c * v + d * x)
  (a * y + b * z + e) (c * y + d * z + f)

compose :: [Transform] -> Transform
compose = foldl' o identity

scale :: Double -> Transform
scale factor = Transform factor 0 0 factor 0 0

rotate :: Double -> Transform
rotate turns = Transform c (-s) s c 0 0
  where
    a = 2 * pi * turns
    c = cos a
    s = sin a

translate :: Double -> Double -> Transform
translate x y = Transform 1 0 0 1 x y

data Point = Point !Double !Double
  deriving (Read, Show)

scalePoint :: Double -> Point -> Point
scalePoint factor (Point x y) = Point (factor * x) (factor * y)

addPoint :: Point -> Point -> Point
addPoint (Point x y) (Point u v) = Point (x + u) (y + v)

distanceSquared :: Point -> Point -> Double
distanceSquared (Point x y) (Point u v) = sqr (x - u) + sqr (y - v)

center :: Transform -> Point
center (Transform _ _ _ _ x y) = Point x y

sizeSquared :: Transform -> Double
sizeSquared (Transform a b _ _ _ _) = sqr a + sqr b

data Box = Box !Double !Double !Double !Double
  deriving (Read, Show)

makeBox :: Double -> Double -> Box
makeBox width height = Box (-width) (-height) width height

scaleBox :: Double -> Box -> Box
scaleBox factor (Box lx ly hx hy) = Box (cx - dx) (cy - dy) (cx + dx) (cy + dy)
  where
    cx = (hx + lx) / 2
    cy = (hy + ly) / 2
    dx = (hx - lx) / 2 * factor
    dy = (hy - ly) / 2 * factor

boxSize :: Box -> Double
boxSize (Box lx ly hx hy) = sqrt (sqr (hx - lx) + sqr (hy - ly))

clampBox :: Box -> Point -> Point
clampBox (Box lx ly hx hy) (Point x y) = Point (clamp x lx hx) (clamp y ly hy)

outsideBox :: Box -> Point -> Double -> Bool
outsideBox (Box lx ly hx hy) (Point ox oy) radiusSquared =
  let r = sqrt radiusSquared
  in  ox + r < lx || hx < ox - r || oy + r < ly || hy < oy - r
