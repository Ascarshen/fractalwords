module GraphGrow.Engine.Grow where

import Data.List (partition)

import GraphGrow.Engine.Geometry
import GraphGrow.Engine.Graph
import GraphGrow.Utils (sqr, SThree(..))

data Context = Context
  { ctxNode :: !Node
  , ctxPath :: [Edge]
  , ctxZoom :: !Double
  , ctxTransform :: !Transform
  }
  deriving (Read, Show)

type ContextPlus = SThree Context Point Double

plus :: Context -> ContextPlus
plus ctx = SThree ctx (center (ctxTransform ctx)) (sizeSquared (ctxTransform ctx))

unplus :: ContextPlus -> Context
unplus (SThree ctx _ _) = ctx

renormalize :: Graph g => g -> Box -> Context -> [Context]
renormalize graph box ctx
  | ctxZoom ctx > 1e16 = []
  | otherwise = case map unplus $ grow graph box (16 * boxSize box) False ctx of
      ls@(_:_) ->
        [ let g = compose . map (edgeTransform graph) . reverse $ ctxPath c
          in  Context
                { ctxNode = ctxNode c
                , ctxPath = []
                , ctxZoom = ctxZoom c * sqrt (sizeSquared g)
                , ctxTransform = ctxTransform ctx `o` g
                }
        | c <- ls ]
      _ -> [ctx]

visible :: Box -> ContextPlus -> Bool
visible box (SThree _ c s) = not (outsideBox box c s)

grow :: Graph g => g -> Box -> Double -> Bool -> Context -> [ContextPlus]
grow graph box threshold keepAll = \ctx -> go [plus ctx]
  where
    thresholdSq = sqr threshold
    small (SThree _ _ sizeSq) = sizeSq < thresholdSq
    next = concatMap (map plus . split graph . unplus)
    go | keepAll = goKeepAll
       | otherwise = goDiscard
    goDiscard [] = []
    goDiscard ss =
      let (smalls, bigs) = (partition small . filter (visible box)) ss
      in  smalls ++ goDiscard (next bigs)
    goKeepAll [] = []
    goKeepAll ss =
      let bigs = (filter (visible box) . filter (not . small)) ss
      in  ss ++ goKeepAll (next bigs)

split :: Graph g => g -> Context -> [Context]
split g ctx =
  [ Context
      { ctxNode = m
      , ctxPath = e : ctxPath ctx
      , ctxZoom = ctxZoom ctx
      , ctxTransform = ctxTransform ctx `o` s
      }
  | SThree e m s <- edgesFrom g (ctxNode ctx)
  ]

zoom :: Transform -> Double -> Context -> Context
zoom z w ctx =
  ctx{ ctxTransform = z `o` ctxTransform ctx, ctxZoom = w * ctxZoom ctx }
