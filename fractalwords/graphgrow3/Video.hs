module Video (videoNew, Video) where

import Control.Monad (when, replicateM_, forM_, unless)
import Control.Monad.IO.Class (liftIO)

import System.IO (hPutStrLn, stderr)

import Data.IORef (IORef, newIORef, readIORef, writeIORef)

import Foreign (nullPtr, castPtr, peek, with, withArray, allocaBytes)
import Foreign.C (withCString, peekCStringLen)

import Data.Map (Map)
import qualified Data.Map.Strict as M

import qualified Graphics.UI.Gtk        as G
import qualified Graphics.UI.Gtk.OpenGL as G

import Graphics.GL

import Linear (M33)

import Paths_graphgrow3 (getDataFileName)
import Types

type Video = [(Int, Double, M33 Double)]

videoNew :: IO (Map RID Video) -> IO G.Window
videoNew transforms = do
  let w = 500
      h = 500
  window <- G.windowNew
  _ <- window `G.on` G.deleteEvent $ do
    liftIO $ G.widgetHide window
    return True
  G.windowSetDefaultSize window w h
  G.windowSetGeometryHints window (Nothing `asTypeOf` Just window) (Just (w, h)) (Just (w, h)) Nothing Nothing Nothing
  canvas <- G.glDrawingAreaNew =<< G.glConfigNew [G.GLModeRGBA, G.GLModeDouble]
  G.widgetSetSizeRequest canvas w h
  let redraw ref = do
        _ <- G.withGLDrawingArea canvas $ \gl -> do
          trs <- transforms
          st <- readIORef ref
          when (vTrs st /= trs) $ do
            writeIORef ref st{ vTrs = trs }
            visualize ref
          when (M.null trs) $ do
            glClear GL_COLOR_BUFFER_BIT
          G.glDrawableSwapBuffers gl
        return True
  _ <- G.onRealize canvas $ do
    _ <- G.withGLDrawingArea canvas $ \gl -> do
      ref <- newIORef =<< initialize
      visualize ref
      G.glDrawableSwapBuffers gl
      G.timeoutAddFull (redraw ref) G.priorityDefaultIdle 100
    return ()
  G.set window [G.windowTitle G.:= "GG#V", G.containerChild G.:= canvas]
  G.widgetShowAll window
  return window

size :: Int
size = 2048

er :: Double
er = 4

data St = St
  { pInitial :: GLuint, pInitial'er, pInitial'rho :: GLint
  , pStep :: GLuint, pStep'er, pStep'count, pStep'ts, pStep'ws, pStep'ls, pStep'layer, pStep'src :: GLint
  , pColour :: GLuint, pColour'src, pColour'speed :: GLint
  , tPing, tPong :: GLuint
  , fBuffer :: GLuint
  , vTrs :: Map RID Video
  }

getUniformLocation :: GLuint -> String -> IO GLint
getUniformLocation program name = withCString name $ glGetUniformLocation program

initialize :: IO St
initialize = do
  pInitial'     <- compileProgram (Just "initial.vert") (Just "initial.frag")
  pInitial'er'  <- getUniformLocation pInitial' "er"
  pInitial'rho' <- getUniformLocation pInitial' "rho"
  pStep'        <- compileProgram (Just "step.vert") (Just "step.frag")
  pStep'er'     <- getUniformLocation pStep' "er"
  pStep'count'  <- getUniformLocation pStep' "count"
  pStep'ts'     <- getUniformLocation pStep' "transform"
  pStep'ws'     <- getUniformLocation pStep' "source"
  pStep'ls'     <- getUniformLocation pStep' "scaleFactor"
  pStep'layer'  <- getUniformLocation pStep' "layer"
  pStep'src'    <- getUniformLocation pStep' "src"
  pColour'      <- compileProgram (Just "colour.vert") (Just "colour.frag")
  pColour'src'  <- getUniformLocation pColour' "src"
  pColour'speed'<- getUniformLocation pColour' "speed"
  tPing'        <- newTex size
  tPong'        <- newTex size
  fBuffer'      <- newFBO
  glClampColor GL_CLAMP_VERTEX_COLOR   (fromIntegral GL_FALSE)
  glClampColor GL_CLAMP_READ_COLOR     (fromIntegral GL_FALSE)
  glClampColor GL_CLAMP_FRAGMENT_COLOR (fromIntegral GL_FALSE)
  return St
    { pInitial = pInitial', pInitial'er = pInitial'er', pInitial'rho = pInitial'rho'
    , pStep = pStep', pStep'er = pStep'er', pStep'count = pStep'count', pStep'ts = pStep'ts', pStep'ws = pStep'ws', pStep'ls = pStep'ls', pStep'layer = pStep'layer', pStep'src = pStep'src'
    , pColour = pColour', pColour'src = pColour'src', pColour'speed = pColour'speed'
    , tPing = tPing', tPong = tPong', fBuffer = fBuffer'
    , vTrs = M.empty
    }

clamp :: Ord a => a -> a -> a -> a
clamp mi ma x = mi `max` x `min` ma

visualize :: IORef St -> IO ()
visualize sR = do
  s0 <- readIORef sR
  glViewport 0 0 (fromIntegral size) (fromIntegral size)
  glLoadIdentity
  glOrtho 0 1 0 1 (-1) 1
  let nlayers = M.size (vTrs s0)
      layers = [0 .. fromIntegral nlayers - 1]
  forM_ layers $ \layer -> do
    bindFBO (fBuffer s0) (tPing s0) (fromIntegral layer)
    glUseProgram (pInitial s0)
    glUniform1f (pInitial'er s0) (realToFrac er)
    glUniform1f (pInitial'rho s0) (realToFrac (maximum (0:[ rho | Just (_, rho, _) <- sequence $ M.lookup layer (vTrs s0) ])))
    unitQuad
    glUseProgram 0
    unbindFBO
    reportErrors "initialize layer"
  let rho = maximum (0:[ rho' | (_, rho', _) <- concat $ M.elems (vTrs s0) ])
      passes = clamp 16 4096 . round . logBase rho $ 0.001
  replicateM_ passes $ do
    s <- readIORef sR
    glBindTexture GL_TEXTURE_2D_ARRAY (tPing s)
    glUseProgram (pStep s)
    glUniform1f (pStep'er s) (realToFrac er)
    glUniform1i (pStep'src s) 0
    forM_ layers $ \layer -> case fmap (fromIntegral . length) $ M.lookup layer (vTrs s) of
      Just count -> do
        bindFBO (fBuffer s) (tPong s) (fromIntegral layer)
        glUniform1i (pStep'count s) count
        withArray (concatMap (\(_,_,m) -> map realToFrac .  concat . toLists $ m) $ vTrs s M.! layer) $ glUniformMatrix3fv (pStep'ts s) count (fromIntegral GL_TRUE)
        withArray (map (\(_,l,_) -> realToFrac l) $ vTrs s M.! layer) $ glUniform1fv (pStep'ls s) count
        withArray (map (\(i,_,_) -> fromIntegral i) $ vTrs s M.! layer) $ glUniform1fv (pStep'ws s) count
        glUniform1f (pStep'layer s) (realToFrac layer)
        unitQuad
        unbindFBO
        reportErrors "step layer"
      Nothing -> return ()
    glUseProgram 0
    glBindTexture GL_TEXTURE_2D_ARRAY 0
    writeIORef sR s{ tPing = tPong s, tPong = tPing s }
    reportErrors "step pass"

  s <- readIORef sR
  glViewport 0 0 500 500
  glBindTexture GL_TEXTURE_2D_ARRAY (tPing s)
  glUseProgram (pColour s)
  glUniform1i (pColour'src s) 0
  glUniform1f (pColour'speed s) (2 * pi / fromIntegral passes)
  fullQuad
  glUseProgram 0
  glBindTexture GL_TEXTURE_2D_ARRAY 0
  reportErrors "draw"

newTex :: Int -> IO GLuint
newTex s = do
  let maxLayers = 4
  t <- with 0 $ \p -> glGenTextures 1 p >> peek p
  glBindTexture GL_TEXTURE_2D_ARRAY t
  glTexImage3D GL_TEXTURE_2D_ARRAY 0 (fromIntegral GL_R32F) (fromIntegral s) (fromIntegral s) maxLayers 0 GL_RED GL_UNSIGNED_BYTE nullPtr
  glTexParameteri GL_TEXTURE_2D_ARRAY GL_TEXTURE_MIN_FILTER (fromIntegral GL_LINEAR)
  glTexParameteri GL_TEXTURE_2D_ARRAY GL_TEXTURE_MAG_FILTER (fromIntegral GL_LINEAR)
  glBindTexture GL_TEXTURE_2D_ARRAY 0
  return t

newFBO :: IO GLuint
newFBO = with 0 $ \p -> glGenFramebuffers 1 p >> peek p

bindFBO :: GLuint -> GLuint -> GLint -> IO ()
bindFBO f t layer = do
  glBindFramebuffer GL_FRAMEBUFFER f
  glFramebufferTextureLayer GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 t 0 layer
  with GL_COLOR_ATTACHMENT0 $ glDrawBuffers 1

unbindFBO :: IO ()
unbindFBO = do
  glFramebufferTextureLayer GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 0 0 0
  glBindFramebuffer GL_FRAMEBUFFER 0

unitQuad :: IO ()
unitQuad = glDrawArrays GL_TRIANGLE_STRIP 0 4

fullQuad :: IO ()
fullQuad = glDrawArrays GL_TRIANGLE_STRIP 0 4

reportErrors :: String -> IO ()
reportErrors s = do
  e <- glGetError
  when (e /= 0) $ do
    hPutStrLn stderr $ s ++ " OpenGL ERROR " ++ show e
    reportErrors s

compileProgram :: Maybe FilePath -> Maybe FilePath -> IO GLuint
compileProgram mV mF = do
  p <- glCreateProgram
  case mV of
    Nothing -> return ()
    Just v -> do
      vert <- glCreateShader GL_VERTEX_SHADER
      source <- readFile =<< getDataFileName v
      shaderSource vert source
      glCompileShader vert
      glAttachShader p vert
  case mF of
    Nothing -> return ()
    Just f -> do
      frag <- glCreateShader GL_FRAGMENT_SHADER
      source <- readFile =<< getDataFileName f
      shaderSource frag source
      glCompileShader frag
      glAttachShader p frag
  glLinkProgram p
  n <- with (0 :: GLint) $ \q -> do
    glGetProgramiv p GL_INFO_LOG_LENGTH q
    peek q
  l <- allocaBytes (fromIntegral n + 1) $ \q -> do
    glGetProgramInfoLog p (fromIntegral n) nullPtr q
    peekCStringLen (castPtr q, fromIntegral n)
  unless (null l) $ do
    hPutStrLn stderr l
  return p

shaderSource :: GLuint -> String -> IO ()
shaderSource shader source = do
  withCString source $ \ptr -> with ptr $ \ptr' ->
    glShaderSource shader 1 (castPtr ptr') nullPtr
