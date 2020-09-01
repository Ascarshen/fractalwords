{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}
module GraphGrow.Operator.Graph (GraphEditor, newGraphEditor) where

import Graphics.UI.Gtk.Toy.Prelude hiding (D, bg)

import Data.Colour.RGBSpace (uncurryRGB)
import Data.Colour.RGBSpace.HSV (hsv)
import Data.Colour.SRGB (sRGB, toSRGBBounded)
import Data.Fixed (mod')

import Data.Default (Default(def))
import Data.Label (get)
import qualified Data.Map as M
import Data.Maybe (maybeToList)

import qualified Graphics.UI.Gtk as G
import Control.Monad (forM_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

import Fractal.GraphGrow.Engine.Geometry (Transform, scale, rotate, translate)
import Fractal.GraphGrow.Engine.Graph (Node(Node))

type GraphEditor = Toy Graph

newGraphEditor :: Node -> ([(Node, Transform)] -> IO ()) -> IO GraphEditor
newGraphEditor node putUpdates = newToy (graph node putUpdates)

setGraphEditorNodes :: GraphEditor -> [Node] -> IO ()
setGraphEditorNodes tb nodes = do
  g <- snd `fmap` readIORef (toyState tb)
  
phi :: Double 
phi = (sqrt 5 + 1) / 2

nodeColour (Node n) = uncurryRGB sRGB $ hsv (360 * ((phi * fromIntegral n) `mod'` 1)) 1 1

type D = Draggable Cairo CairoDiagram -- every thing needs be draggable

-- knob and link ids
type KID = Int 
type LID = (KID, KID)

-- things
data Thing = Background {-nextKnobID-}KID D | Knob {-isFixedPosition-}Bool KID D D | Link LID D D Node

-- visual representation
bg n = mkDraggable (r2 (300, 300)) $ rect 600 600 # fc (blend (1/16) (nodeColour n) white)
knob n fixed p =
    ( mkDraggable (r2 p) $ shape  5 # lc black # fc col
    , mkDraggable (r2 p) $ shape 12 # fcA (col `withOpacity` 0.5)
    )
  where
    col = nodeColour n
    shape = if fixed then square . ((pi/2) *) else circle
link n p@(x0,y0) q@(x1,y1) =
    ( mkDraggable (r2((x0+x1)/2,(y0+y1)/2)) $ circle 5 # lc black # fc col
    , mkDraggable (r2(0,0)) $ p2 p ~~ p2 q # stroke # lc col
    )
  where
    col = nodeColour n

-- extract diagrams
tDiagram (Background _ d)   = [d]
tDiagram (Knob _ _ d1 d2)   = [d1, d2]  -- d1 is inner core, d2 is outer ring
tDiagram (Link _   d1 d2 _) = [d1, d2]  -- d1 is inner core, d2 is line

-- what did a mouse event do
data Act
  = Create KID
  | Delete KID
  | MoveStart KID
  | Moving    KID
  | MoveStop  KID
  | LinkStart        KID  (Double, Double)
  | Linking               (Double, Double)
  | LinkStop (Maybe (KID, (Double, Double)))

-- what mode are we in
data Mode
  = MMove KID                                   -- moving a knob
  | MLink KID (Double, Double) (Double, Double) -- creating a link

data Graph = Graph
  { gNode     :: Node
  , gGetNodes :: IO [Node]
  , gPutEdges :: [(Node, Transform)] -> IO ()
  , gMode     :: Maybe Mode
  , gThings   :: [Thing]
  }

-- update links when knobs are moved or deleted
resort g@Graph{ gThings = ts } = g{ gThings = ts' }
  where
    knobs = [ n | n@(Knob      {}) <- ts ]
    links = [ l | l@(Link      {}) <- ts ]
    backg = [ b | b@(Background{}) <- ts ]
    positions = M.fromList [ (i, unr2 $ get dragOffset d) | Knob _ i d _ <- knobs ]
    links' =
      [ Link ij d1 d2 n
      | Link ij@(i, j) _ _ n <- links
      , (d1, d2) <- maybeToList $
          liftA2 (link n) (M.lookup i positions) (M.lookup j positions)
      ]
    ts' = knobs ++ links' ++ backg

-- mouse handling
-- linking knobs
tMouse _ (Just(MLink j q _))(Just(False, 0)) p t@(Background _ _    )                                      = (Just $ LinkStop Nothing      , [t]) -- note: Background is the last Thing in the list
tMouse _ (Just(MLink j q _))(Just(False, 0)) p t@(Knob fixed i d1 d2) | j /= i && clickInside d2 (p2 p)    = (Just $ LinkStop (Just (i, p)), [t])
tMouse _ (Just(MLink j q _))(Just(False, 0)) p t                                                           = (Nothing                      , [t])
tMouse _ (Just(MLink j q _))_                p t                                                           = (Just $ Linking     p         , [t])
-- moving knobs
tMouse _ Nothing         m@(Just (True,  0)) p t@(Knob fixed i d1 d2) | clickInside d1 (p2 p) && not fixed = (Just $ MoveStart i  , [Knob False i (mouseDrag m p d1) (mouseDrag m p d2)])
                                                                      | clickInside d2 (p2 p)              = (Just $ LinkStart i p         , [t])
tMouse _ (Just(MMove j)) m@(Just (False, 0)) p   (Knob False i d1 d2) | j == i                             = (Just $ MoveStop  i  , [Knob False i (mouseDrag m p d1) (mouseDrag m p d2)])
tMouse _ (Just(MMove j)) m                   p   (Knob False i d1 d2) | j == i                             = (Just $ Moving    i  , [Knob False i (mouseDrag m p d1) (mouseDrag m p d2)])
-- creating knobs
tMouse c Nothing           (Just (True,  0)) p   (Background i d)                                          = (Just $ Create    i  , [uncurry (Knob False i) (knob c False p), Background (i + 1) d])
tMouse _ Nothing           (Just (True,  1)) p   (Knob False i d1 d2) | clickInside d1 (p2 p)              = (Just $ Delete    i  , [])
tMouse _ _currentMode      _mouseEvent       _ t@_thingToTest                                              = (Nothing             , [t])

-- concatMap until a thing signals something was done, then just copy the rest
tMouses _ _ _ _ a@(Just _) ws = (a, ws)
tMouses _ _ _ _ Nothing [] = (Nothing, [])
tMouses c d m p Nothing (w:ws) =
  let (a, ws') = tMouse c d m p w
  in  (ws' ++) `fmap` tMouses c d m p a ws

type instance V Graph = R2
  
graph node getNodes putUpdates = Graph node getNodes putUpdates Nothing
  [ uncurry (Knob True 1) (knob node True (500, 300))
  , uncurry (Knob True 0) (knob node True (100, 300))
  , Background 2 (bg node)
  ]

instance Interactive Graph where
  mouse = simpleMouse $ \m p g@(Graph{ gNode = c, gMode = d, gThings = ts}) -> resort $ case (d, tMouses c d m p Nothing ts) of
    -- moving knobs
    (Nothing           , (Just (MoveStart i           ), ts'))          -> g{ gThings = ts', gMode = Just (MMove i) }
    (Just (MMove j)    , (Just (Moving    i           ), ts')) | j == i -> g{ gThings = ts', gMode = Just (MMove i) }
    (Just (MMove j)    , (Just (MoveStop  i           ), ts')) | j == i -> g{ gThings = ts', gMode = Nothing }
    -- linking knobs
    (Nothing,            (Just (LinkStart i p         ), ts'))          -> g{ gThings = ts', gMode = Just (MLink i p p) }
    (Just (MLink j q _), (Just (Linking     p         ), ts'))          -> g{ gThings = ts', gMode = Just (MLink j q p) }
    (Just (MLink j q _), (Just (LinkStop (Just (i, p))), ts')) | j /= i -> g{ gThings = uncurry (Link (j, i)) (link c q p) c: ts', gMode = Nothing }
    (Just (MLink j q _), (Just (LinkStop Nothing      ), ts'))          -> g{ gThings = ts', gMode = Nothing }
    -- everything else
    (_, (_, ts')) -> g{ gThings = ts', gMode = Nothing }

instance Diagrammable Cairo Graph where
  diagram (Graph{ gMode = m, gThings = ts}) = mconcat . map diagram $ modeDiagram ++ concatMap tDiagram ts
    where
      modeDiagram = case m of
        Just (MLink _ p q) -> [mkDraggable (r2(0,0)) $ p2 p ~~ p2 q # stroke # lc black]
        _ -> []

instance GtkInteractive Graph where
  display = displayDiagram diagram

main :: IO ()
main = do
  G.initGUI
  graphsRef <- newIORef M.empty
  window <- G.windowNew
  G.windowSetDefaultSize window 600 600
  box <- G.hButtonBoxNew
  
  button <- G.buttonNewWithLabel "+"
  button `G.on` G.buttonActivated $ do
    graphs <- readIORef graphsRef
    let n = maybe (Node 0) ((\(Node m) -> Node (m+1)) . fst . fst) (M.maxViewWithKey graphs)
        gcol = uncurryRGB G.Color $ toSRGBBounded (nodeColour n)
    g <- newGraphEditor n (M.keys `fmap` readIORef graphsRef) (\_ -> return ())
    w <- G.windowNew
    G.windowSetGeometryHints w (Nothing `asTypeOf` Just w) (Just (600, 600)) (Just (600, 600)) Nothing Nothing Nothing
    G.set w $ [G.containerChild G.:= toyWindow g]
    writeIORef graphsRef (M.insert n (w, g) graphs)
    b <- G.buttonNew
    b `G.on` G.buttonActivated $ G.windowPresent{-WithTime-} w
    forM_ [G.StateNormal, G.StateActive, G.StatePrelight, G.StateSelected, G.StateInsensitive] $ \s ->
      G.widgetModifyBg b s gcol
    G.containerAdd box b
    G.widgetShowAll window
    G.widgetShowAll w

  G.containerAdd box button
  G.containerAdd window box
  G.widgetShowAll window
  G.mainGUI
