module Main (main) where

    import Control.Concurrent (forkIO, killThread)
    import Control.Concurrent.BoundedChan (newBoundedChan, readChan, writeChan)
    import Control.Monad (join, replicateM_, when)
    import Data.IORef (newIORef, readIORef, writeIORef)
    import qualified Data.Map as M
    import Data.Maybe (fromMaybe)
    import System.Environment (getArgs)
    import System.Exit (exitFailure)
    import System.IO (hPutStrLn, stderr)
    import System.Random (newStdGen)
    
    import qualified Graphics.UI.Gtk        as G
    import qualified Graphics.UI.Gtk.OpenGL as G
    import qualified System.Glib.GDateTime  as G
    
    import Fractal.GraphGrow.Engine.Geometry
    import Fractal.GraphGrow.Engine.Graph
    import Fractal.GraphGrow.Engine.Zoomer
    import Fractal.GraphGrow.Text.Parse
    import Fractal.GraphGrow.Text.Compile
    import Fractal.GraphGrow.GUI.DrawPoints
    import Fractal.GraphGrow.GUI.FrameSync
    import Fractal.GraphGrow.Utils
    import Fractal.GraphGrow.Analysis.Statistics
    
    readGraph :: Maybe FilePath -> Maybe String -> IO (Maybe (VGraph, Node))
    readGraph mf mw = do
      s <- case mf of
        Nothing -> getContents
        Just f -> readFile f
      return $ do
        (ws, g) <- compileText `fmap` parseText s
        v <- vectorize (annotate g)
        let n = fromMaybe (Node 0) ((`M.lookup` ws) =<< mw)
        return (v, n)
    
    errLn :: String -> IO ()
    errLn = hPutStrLn stderr
    
    usageMessage :: IO ()
    usageMessage = errLn "usage: FIXME"
    
    inputMessage :: IO ()
    inputMessage = errLn "bad input"
    
    vsyncMessage :: Double -> IO ()
    vsyncMessage fps = do
      errLn$"insane fps: " ++ show fps
      errLn "enable OpenGL sync-to-vblank and retry"
    
    statsMessage :: Statistics -> IO ()
    statsMessage s = do
      let frames = statCount s
          totalTime = (s1 . fst . statFrameTime) s
          fps = fromIntegral frames / totalTime
          pretty ss = "\t" ++ show mi ++ "\t" ++ show mean ++ "\t" ++ show ma ++ "\t" ++ show dev
            where
              (mi, ma) = fromMaybe (0/0, 0/0) . getMinMax . snd $ ss
              mean = getMean (fst ss)
              dev = getStdDev (fst ss)
      errLn "[ STATISTICS"
      errLn $ "FPS\t" ++ show fps ++ "\t( " ++ show frames ++ " f / " ++ show totalTime ++ " s ) [ " ++ show (statMissed s) ++ " MISSED ]"
      errLn $ "FRAME" ++ pretty (statFrameTime s)
      errLn $ "SPARE" ++ pretty (statSpareTime s)
      errLn $ "DELTA" ++ pretty (statOk s)
      errLn "]"
    
    insaneFPS :: Double -> Bool
    insaneFPS fps = fps < 20 || 200 < fps
    
    parseArgs :: [String] -> Maybe Config
    parseArgs [file] = Just defaultConfig{ configGraph = if file == "-" then Nothing else Just file }
    parseArgs [file, node] = Just defaultConfig{ configGraph = if file == "-" then Nothing else Just file, configNode = Just node }
    parseArgs _ = Nothing
    
    data Config = Config
      { configGraph :: Maybe FilePath
      , configNode :: Maybe String
      , configWidth :: Int
      , configHeight :: Int
      , configMinQuality :: Double
      , configMaxQuality :: Double
      , configVerbose :: Bool
      }
    
    defaultConfig :: Config
    defaultConfig = Config
      { configGraph = Nothing
      , configNode = Nothing
      , configWidth = 1024
      , configHeight = 576
      , configMinQuality = recip 16
      , configMaxQuality = 16
      , configVerbose = False
      }
    
    configSize :: Config -> Double
    configSize config = sqrt (fromIntegral $ sqr (configWidth config) + sqr (configHeight config))
    
    newtype Continue = Continue{ runContinue :: IO Continue }
    
    skip :: Continue
    skip = Continue (return skip)
    
    abort :: IO Continue
    abort = exitFailure >> return skip
    
    main :: IO ()
    main = do
      args <- getArgs
      case parseArgs args of
        Nothing -> usageMessage
        Just config -> do
          mgraph <- readGraph (configGraph config) (configNode config)
          case mgraph of
            Nothing -> inputMessage
            Just (graph, node) -> do
              let initial = defaultZoomerOptions graph node
              withGL "GraphGrow" (configWidth config) (configHeight config) $ \getTime swapBuffers -> do
                (fps, frameSync) <- newFrameSync getTime swapBuffers defaultFrameSyncOptions
                if insaneFPS fps
                  then vsyncMessage fps >> abort
                  else withPointDraw (zoomerOuterBox initial) (\s -> errLn s >> abort) $ \draw -> do
                    worker <- newIORef Nothing
                    let queueSize = 2
                        restart = do
                          maybe (return ()) killThread =<< readIORef worker
                          qualityChan <- newBoundedChan queueSize
                          pointsChan <- newBoundedChan queueSize
                          writeIORef worker . Just =<< forkIO . workerThread (readChan qualityChan) (writeChan pointsChan) . zoomer initial =<< newStdGen
                          replicateM_ queueSize $ writeChan qualityChan (configMaxQuality config)
                          loop (readChan pointsChan) (writeChan qualityChan)
                        workerThread getQuality putPoints = go
                          where
                            go zoom = do
                              quality <- getQuality
                              let pixelSize = quality * boxSize (zoomerOuterBox initial) / configSize config
                                  Zoomed points zoom' = zoom pixelSize
                                  sanePoints = filter saneSize points
                                  saneSize (STwo _ sizeSq) = sizeSq < sqr (boxSize (zoomerOuterBox initial)) / 4
                              if null sanePoints then putPoints [] else do
                                () <- putPoints =<< flattenPoints sanePoints
                                go zoom'
                        loop getPoints putQuality = go (configMaxQuality config)
                          where
                            go quality = do
                              points <- getPoints
                              if null points then restart else do
                                _drawnCount <- draw points
                                (deadline, stats) <- frameSync
                                when (configVerbose config && statCount stats `mod` 256 == 0) $
                                  statsMessage stats
                                let e = negate . recip . fromIntegral $ 16 * queueSize
                                    slew = case deadline of
                                      Missed -> sqrt 2
                                      Ok k
                                        | k > 1     -> k ** e
                                        | otherwise -> k ** e
                                    slew' = clamp slew (sqrt 0.5) (sqrt 2)
                                    quality' = clamp (quality * slew') (configMinQuality config) (configMaxQuality config)
                                putQuality quality'
                                return $ Continue (go quality')
                    restart
    
    withGL :: String -> Int -> Int -> (IO Double -> IO () -> IO Continue) -> IO ()
    withGL title w h go = do
      _ <- G.initGUI
      window <- G.windowNew
      _ <- G.onDestroy window G.mainQuit
      G.windowSetDefaultSize window w h
      G.windowSetGeometryHints window (Nothing `asTypeOf` Just window) (Just (w, h)) (Just (w, h)) Nothing Nothing Nothing
      canvas <- G.glDrawingAreaNew =<< G.glConfigNew [G.GLModeRGBA, G.GLModeDouble]
      G.widgetSetSizeRequest canvas w h
      continue <- newIORef skip
      swapBuffersR <- newIORef (return ())
      let getTime = do
            G.GTimeVal sec usec <- G.gGetCurrentTime
            return $ fromIntegral sec + fromIntegral usec / 1000000
          swapBuffers = join (readIORef swapBuffersR)
          tick = do
            _ <- G.withGLDrawingArea canvas $ \gl -> do
              writeIORef swapBuffersR (G.glDrawableSwapBuffers gl)
              writeIORef continue =<< runContinue =<< readIORef continue
              writeIORef swapBuffersR (return ())
            return True
      _ <- G.onRealize canvas $ do
        _ <- G.withGLDrawingArea canvas $ \gl -> do
          writeIORef swapBuffersR (G.glDrawableSwapBuffers gl)
          writeIORef continue =<< go getTime swapBuffers
          writeIORef swapBuffersR (return ())
          G.timeoutAddFull tick G.priorityDefaultIdle 1
        return ()
      event <- G.eventBoxNew
      _ <- G.onKeyPress event $ \_ -> G.mainQuit >> return True
      G.set event  [G.containerBorderWidth G.:= 0, G.containerChild G.:= canvas]
      G.set window [G.windowTitle G.:= title, G.containerChild G.:= event]
      G.widgetShowAll window
      G.mainGUI
    