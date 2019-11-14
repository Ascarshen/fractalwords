{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GraphGrow.Engine.Graph where

import Data.Ix (Ix(..))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Data.Word (Word32)
import Foreign.Storable (Storable(..))

import GraphGrow.Engine.Geometry
import GraphGrow.Utils (SThree(..))

-- Word32 is probably enough, as each edge needs ~56 bytes...
newtype Node = Node Word32 deriving (Read, Show, Ix, Eq, Ord, Storable)
newtype Edge = Edge Word32 deriving (Read, Show, Ix, Eq, Ord, Storable)

class Graph g where
  edgesFrom :: g -> Node -> [SThree Edge Node Transform]
  edgeTransform :: g -> Edge -> Transform

data MGraph = MGraph
  { mGraph :: Map Node [(Node, Transform)]
  }

annotate :: MGraph -> MMGraph
annotate g = MMGraph
    { mNodes = M.fromListWith (++) [ (source, [edge]) | (edge, (source, _)) <- edges ]
    , mEdges = M.fromList [ (edge, tt) | (edge, (_, tt)) <- edges ]
    }
  where
    edges = map Edge [0..] `zip` [ (source, (target, transform)) | (source, tts) <- M.toAscList (mGraph g), (target, transform) <- tts ]

data MMGraph = MMGraph
  { mNodes :: Map Node [Edge]
  , mEdges :: Map Edge (Node, Transform)
  }

instance Graph MMGraph where
  edgesFrom g n = catMaybes
    [ (\(m,t) -> SThree e m t) `fmap` M.lookup e (mEdges g)
    | e <- fromMaybe [] (M.lookup n (mNodes g))
    ]
  edgeTransform g e = snd $ mEdges g M.! e

vectorize :: MMGraph -> Maybe VGraph
vectorize g
  | dense = Just VGraph
      { vNodeOffset = V.fromList . map (Offset . fromIntegral) . init . scanl (+) 0 . map (length . snd) . M.toAscList . mNodes $ g
      , vNodeEdges  = V.fromList . concatMap snd . M.toAscList . mNodes $ g
      , vEdgeTargets = V.fromList . map (fst . snd) . M.toAscList . mEdges $ g
      , vEdgeTransforms = V.fromList . map (snd . snd) . M.toAscList . mEdges $ g
      }
  | otherwise = Nothing
  where
    dense = fmap (fst . fst) (M.minViewWithKey (mNodes g)) == Just (Node 0)
        &&  fmap (fst . fst) (M.maxViewWithKey (mNodes g)) == Just (Node (fromIntegral (M.size (mNodes g) - 1)))
        &&  fmap (fst . fst) (M.minViewWithKey (mEdges g)) == Just (Edge 0)
        &&  fmap (fst . fst) (M.maxViewWithKey (mEdges g)) == Just (Edge (fromIntegral (M.size (mEdges g) - 1)))

newtype Offset = Offset Word32 deriving (Read, Show, Ix, Eq, Ord, Storable)

data VGraph = VGraph
  { vNodeOffset     :: Vector Offset     -- indexed by Node
  , vNodeEdges      :: Vector Edge       -- indexed by Offset
  , vEdgeTargets    :: Vector Node       -- indexed by Edge
  , vEdgeTransforms :: Vector Transform  -- indexed by Edge
  }

instance Graph VGraph where
  edgesFrom g (Node n) = fromMaybe [] $ do
    Offset start <- vNodeOffset g V.!? fromIntegral n
    let Offset end = fromMaybe (Offset . fromIntegral . V.length . vNodeEdges $ g) $ vNodeOffset g V.!? (fromIntegral n + 1)
        go off =
          let edge@(Edge e) = vNodeEdges g V.! off
              e' = fromIntegral e
              target = vEdgeTargets g V.! e'
              transform = vEdgeTransforms g V.! e'
          in  SThree edge target transform
    return $ map go [fromIntegral start .. fromIntegral end - 1]
  edgeTransform g (Edge e) = vEdgeTransforms g V.! fromIntegral e
