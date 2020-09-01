{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts, ForeignFunctionInterface #-}
module Main (main) where

import Graphics.UI.Toy.Gtk.Prelude hiding (D, bg, clamp, arrow, size, with)

import qualified Graphics.UI.Gtk        as G
import qualified Graphics.UI.Gtk.OpenGL as G


import Control.Monad (when, replicateM_, forM_)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, atomicModifyIORef)

import Data.Colour.RGBSpace (uncurryRGB)
import Data.Colour.RGBSpace.HSV (hsv)
import Data.Colour.SRGB (sRGB, toSRGBBounded)
import Data.Fixed (mod')

import Linear (V2, V3, M33, (!*!), norm, (^-^), inv33)
import Data.Foldable (toList)

import Control.Lens (view)
import Data.Map (Map)
import qualified Data.Map.Strict as M
import qualified Data.Map.Strict.Merge as M
import Data.Maybe (maybeToList)

import System.IO (hFlush, stdout)

import Types
import Audio
import Video

type D = Draggable CairoDiagram -- every thing needs be draggable

mkD :: V2 Double -> CairoDiagram -> D
mkD = mkDraggable

-- things
data Thing = Background {-nextNodeID-}NID D | Node {-isFixedPosition-}Bool NID D D | Link LID D D D RID

-- visual representation
bg :: Int -> D
bg r = mkDraggable (r2 (250, 250)) $ (rect 500 500 # fc (blend 0.8 white (ruleColour r)))

node :: Int -> Bool -> V2 Double -> (D, D)
node r fixed p =
    ( mkDraggable p $ shape  5 # lc black # fc col
    , mkDraggable p $ shape 12 # fcA (col `withOpacity` 0.5)
    )
  where col = ruleColour r ; shape = if fixed then square . ((pi/2) *) else circle

link :: Int -> V2 Double -> V2 Double -> (D, D, D)
link r p@(V2 x0 y0) q@(V2 x1 y1) =
    ( mkD pc $ circle 5 # lc black # fc col
    , mkD pa $ arrow # scale 3 # strokeLocTrail # lc black # fc col # rotate a
    , mkD (V2 0 0) $ P p ~~ P q # strokeP # lc col
    )
  where
    col = ruleColour r
    pc@(V2 cx cy) = V2 ((x0+x1)/2) ((y0+y1)/2)
    pa = V2 (cx + 16 * dx) (cy + 16 * dy)
    ux = x1 - x0
    uy = y1 - y0
    a = (realToFrac $ atan2 uy ux) @@ rad
    l = case sqrt (ux * ux + uy * uy) of
      x | x == 0 -> 1
        | otherwise -> x
    dx =   uy / l
    dy = - ux / l
    arrow = (`at` p2(0,0)) . closeTrail . fromVertices . map p2 $ [(-3,-1),(1,-1),(1,-2),(3,0),(1,2),(1,1),(-3,1)]

-- extract diagrams
tDiagram :: Thing -> [D]
tDiagram (Background _ d)   = [d]
tDiagram (Node _ _ d1 d2)   = [d1, d2]  -- d1 is inner core, d2 is outer ring
tDiagram (Link _   d1 d2 d3 _) = [d1, d2, d3]  -- d1 is inner core, d3 is line, d2 is arrow

-- what did a mouse event do
data Act
  = Create NID
  | Delete NID
  | MoveStart NID
  | Moving    NID
  | MoveStop  NID
  | LinkStart        NID  (V2 Double)
  | Linking               (V2 Double)
  | LinkStop (Maybe (NID, (V2 Double)))
  | Adjust LID
  | ReverseLink LID
  | DeleteLink LID

-- what mode are we in
data Mode
  = MMove NID                         -- moving a node
  | MLink NID (V2 Double) (V2 Double) -- creating a link

data State = State (IORef (Map RID (Maybe G.Window, Toy State), IORef (Map RID Video, Map RID Audio))) RID (Maybe Mode) [Thing] Video Audio

-- update links when nodes are moved or deleted
resort :: [RID] -> State -> State
resort rs (State gs r m ts _ _) = State gs r m ts' trs adjs
  where
    adjs = (length nodes,
      [ (i, j, lr, s, x)
      | Link (i, j) _ _ _ lr <- links'
      , let Just p0 = M.lookup i positions
      , let Just p1 = M.lookup j positions
      , let scale = norm (node0Position ^-^ node1Position)
      , let s = norm (p0 ^-^ p1) / scale
      , let x = (view _x (lerp 0.5 p0 p1 ^-^ node0Position) / scale - 0.5) * 0.5 + 0.5
      ])
    trs =
      [ (c, scaleFactor, transformMatrix)
      | Link ij@(i, j) _ _ _ lr <- links'
      , let c = if lr `elem` rs then lr else 0
      , let Just p0 = M.lookup i positions
      , let Just p1 = M.lookup j positions
      , let (scaleFactor, transformMatrix) = linkTransformation p0 p1
      ]
    nodes = [ n | n@(Node      {}) <- ts ]
    -- strip multiple links between the same pair of nodes (FIXME is this the best way?)
    links = (M.elems . M.fromList) [ (sortPair ij, l) | l@(Link ij _ _ _ _) <- ts ]
    backg = [ b | b@(Background{}) <- ts ]
    positions = M.fromList [ (i, view dragOffset d) | Node _ i d _ <- nodes ]
    links' =
      [ Link ij d1 d2 d3 c
      | Link ij@(i, j) _ _ _ lr <- links
      , let c = if lr `elem` rs then lr else 0
      , (d1, d2, d3) <- maybeToList $
          liftA2 (link c) (M.lookup i positions) (M.lookup j positions)
      ]
    ts' = nodes ++ links' ++ backg

linkTransformation :: V2 Double -> V2 Double -> (Double, M33 Double)
linkTransformation p0@(V2 x0 y0) p1@(V2 x1 y1) = (scaleFactor, inv33 matrix)
  where
    scale = norm (node0Position ^-^ node1Position)
    scaleFactor = norm (p0 ^-^ p1) / scale
    angle = -atan2 (y1 - y0) (x1 - x0)
    translation = (((p0 + p1)^/2) ^-^ ((node0Position + node1Position)^/2)) ^/ scale
    matrix = transformRST angle scaleFactor translation

transformRST :: Double -> Double -> V2 Double -> M33 Double
transformRST a l (V2 x y) = V3 (V3 c s x) (V3 (-s) c y) (V3 0 0 1)
  where
    c = l * cos a
    s = l * sin a


-- mouse handling
tMouse :: Int -> Maybe Mode -> Maybe (Bool, Int) -> V2 Double -> Thing -> (Maybe Act, [Thing])
-- linking nodes
tMouse _ (Just(MLink j q _))(Just(False, 0)) p t@(Background _ _    )                                      = (Just $ LinkStop Nothing      , [t]) -- note: Background is the last Thing in the list
tMouse _ (Just(MLink j q _))(Just(False, 0)) p t@(Node fixed i d1 d2) | j /= i && clickInside d2 (P p)     = (Just $ LinkStop (Just (i, p)), [t])
tMouse _ (Just(MLink j q _))(Just(False, 0)) p t                                                           = (Nothing                      , [t])
tMouse _ (Just(MLink j q _))_                p t                                                           = (Just $ Linking     p         , [t])
-- adjusting links
tMouse _ Nothing           (Just (True,  0)) p   (Link i d1 d2 d3 r)  | clickInside d1 (P p)               = (Just $ Adjust      i, [Link i d1 d2 d3 (r + 1)])
                                                                      | clickInside d2 (P p)               = (Just $ ReverseLink i, [Link (swapPair i) d1 d2 d3 r])
tMouse _ Nothing           (Just (True,  1)) p   (Link i d1 d2 d3 _)  | clickInside d1 (P p)               = (Just $ DeleteLink  i, [])
-- moving nodes
tMouse _ Nothing         m@(Just (True,  0)) p t@(Node fixed i d1 d2) | clickInside d1 (P p) && not fixed  = (Just $ MoveStart i  , [Node False i (mouseDrag m q d1) (mouseDrag m q d2)])
                                                                      | clickInside d2 (P p)               = (Just $ LinkStart i (view dragOffset d1), [t])                         where q = clamp' p
tMouse _ (Just(MMove j)) m@(Just (False, 0)) p   (Node False i d1 d2) | j == i                             = (Just $ MoveStop  i  , [Node False i (mouseDrag m q d1) (mouseDrag m q d2)]) where q = clamp' p
tMouse _ (Just(MMove j)) m                   p   (Node False i d1 d2) | j == i                             = (Just $ Moving    i  , [Node False i (mouseDrag m q d1) (mouseDrag m q d2)]) where q = clamp' p
-- creating nodes
tMouse c Nothing           (Just (True,  0)) p   (Background i d)                                          = (Just $ Create    i  , [uncurry (Node False i) (node c False p), Background (i + 1) d])
tMouse _ Nothing           (Just (True,  1)) p   (Node False i d1 d2) | clickInside d1 (P p)               = (Just $ Delete    i  , [])
tMouse _ _currentMode      _mouseEvent       _ t@_thingToTest                                              = (Nothing             , [t])

clamp' :: V2 Double -> V2 Double
clamp' (V2 x y) = V2 (c x) (c y) where c z = 5 `max` z `min` 495

-- concatMap until a thing signals something was done, then just copy the rest
tMouses :: Int -> Maybe Mode -> Maybe (Bool, Int) -> V2 Double -> Maybe Act -> [Thing] -> (Maybe Act, [Thing])
tMouses _ _ _ _ a@(Just _) ws = (a, ws)
tMouses _ _ _ _ Nothing [] = (Nothing, [])
tMouses c d m p Nothing (w:ws) =
  let (a, ws') = tMouse c d m p w
  in  (ws' ++) `fmap` tMouses c d m p a ws

type instance N State = Double
type instance V State = V2
  
graph :: IORef (Map RID (Maybe G.Window, Toy State), IORef (Map RID Video, Map RID Audio)) -> RID -> State
graph gs r = State gs r Nothing
  [ uncurry (Node True 0) (node r True node0Position)
  , uncurry (Node True 1) (node r True node1Position)
  , Background 2 (bg r)
  ] [] (2, [])

node0Position :: V2 Double
node0Position = V2 100 250

node1Position :: V2 Double
node1Position = V2 400 250

instance Interactive Gtk State where
  mouse _m _p s@(State g0 c0 _ _ _ _) = do
    rs <- (M.keys . fst) `fmap` readIORef g0
    s' <- simpleMouse ( \m p (State g c d ts trs adjs) -> resort rs $ case (d, tMouses c d m p Nothing ts) of
        -- moving nodes
        (Nothing           , (Just (MoveStart i           ), ts'))          -> State g c (Just (MMove i)) ts' trs adjs
        (Just (MMove j)    , (Just (Moving    i           ), ts')) | j == i -> State g c (Just (MMove i)) ts' trs adjs
        (Just (MMove j)    , (Just (MoveStop  i           ), ts')) | j == i -> State g c Nothing          ts' trs adjs
        -- linking nodes
        (Nothing,            (Just (LinkStart i p         ), ts'))          -> State g c (Just (MLink i p p)) ts' trs adjs
        (Just (MLink j q _), (Just (Linking     p         ), ts'))          -> State g c (Just (MLink j q p)) ts' trs adjs
        (Just (MLink j q _), (Just (LinkStop (Just (i, p))), ts')) | j /= i -> State g c Nothing (uncurry3 (Link (j, i)) (link c q p) c : ts') trs adjs
        (Just (MLink j q _), (Just (LinkStop Nothing      ), ts'))          -> State g c Nothing ts' trs adjs
        -- everything else
        (_, (_, ts')) -> State g c Nothing ts' trs adjs
      ) _m _p s
    case s' of
      State g1 _ _ _ trs adjs -> do
        trsref <- snd `fmap` readIORef g1
        (trsmap, adjmap) <- readIORef trsref
        writeIORef trsref (M.insert c0 trs trsmap, M.insert c0 adjs adjmap)
    return s'

instance Diagrammable Cairo V2 Double Any State where
  diagram (State _ _ m ts _ _) = mconcat . map diagram $ modeDiagram ++ concatMap tDiagram ts
    where
      modeDiagram = case m of
        Just (MLink _ p q) -> [mkD (V2 0 0) $ P p ~~ P q # strokeP # lc black]
        _ -> []

instance GtkDisplay State where
  display = displayDiagram diagram

main :: IO ()
main = do
  G.initGUI
  transforms <- newIORef (M.empty, M.empty)
  graphsRef <- newIORef (M.empty, transforms)
  window <- G.windowNew
  G.onDestroy window G.mainQuit
  G.set window [G.windowTitle G.:= "GG"]
  G.windowSetDefaultSize window 10 10
  G.windowSetTypeHint window G.WindowTypeHintDialog
  box <- G.hButtonBoxNew

  vis <- videoNew (fst <$> readIORef transforms)
  vbutton <- G.buttonNewWithLabel "V"
  vbutton `G.on` G.buttonActivated $ do
    G.windowPresent vis
    return ()
  G.containerAdd box vbutton

  (audio, audioQuit) <- audioNew (snd <$> readIORef transforms)
  G.timeoutAddFull (audio >> return True) G.priorityDefaultIdle 100

  button <- G.buttonNewWithLabel "+"
  button `G.on` G.buttonActivated $ do
    graphs <- fst `fmap` readIORef graphsRef
    let r = maybe 0 (succ . fst . fst) (M.maxViewWithKey graphs)
        gcol = uncurryRGB G.Color $ toSRGBBounded (ruleColour r)
    g <- newToy (graph graphsRef r)
    writeIORef graphsRef (M.insert r (Nothing, g) graphs, transforms)
    b <- G.buttonNew
    forM_ [G.StateNormal, G.StateActive, G.StatePrelight, G.StateSelected, G.StateInsensitive] $ \s ->
      G.widgetModifyBg b s gcol
    b `G.on` G.buttonActivated $ do
      graphs <- fst `fmap` readIORef graphsRef
      case M.lookup r graphs of
        Just (Just w,  g) -> G.windowPresent{-WithTime-} w
        Just (Nothing, g) -> do
          w <- G.windowNew
          w `G.on` G.deleteEvent $ do
            liftIO $ G.widgetHide w
            return True
          writeIORef graphsRef (M.insert r (Just w, g) graphs, transforms)
          G.windowSetGeometryHints w (Nothing `asTypeOf` Just w) (Just (500, 500)) (Just (500, 500)) Nothing Nothing Nothing
          G.set w [G.containerChild G.:= toyWindow g, G.windowTitle G.:= "GG#" ++ show r]
          G.widgetShowAll w
        _ -> return ()
    G.containerAdd box b
    G.widgetShowAll window

  G.containerAdd box button
  G.containerAdd window box
  G.widgetShowAll window

  G.mainGUI
  audioQuit

phi :: Double
phi = (sqrt 5 + 1) / 2

ruleColour :: Int -> Colour Double
ruleColour r = uncurryRGB sRGB $ hsv (360 * ((phi * fromIntegral r) `mod'` 1)) 1 1
