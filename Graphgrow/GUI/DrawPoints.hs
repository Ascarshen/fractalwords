module GraphGrow.GUI.DrawPoints
  ( withPointDraw
  , flattenPoints
  ) where

import Control.Monad (forM_)
import Foreign (Ptr, allocaArray, castPtr, pokeElemOff, with, withArray, peek, nullPtr, sizeOf)
import Foreign.C (withCString, peekCStringLen)
import Foreign.ForeignPtr.Safe (ForeignPtr, mallocForeignPtrBytes, withForeignPtr)
import Graphics.GL
import Unsafe.Coerce (unsafeCoerce)

import GraphGrow.Engine.Geometry (Point(..), Box(..))
import GraphGrow.Utils (STwo(..), sum')

floatToGLfloat :: Float -> GLfloat
floatToGLfloat = unsafeCoerce

flatten :: [STwo Point Double] -> [GLfloat]
flatten fs = map (floatToGLfloat . realToFrac) $ concat [[x, y, z, 2 * pi * (t + 0.5) / 4] | STwo (Point x y) z <- fs, t <- [0,1,2,3]]

flattenPoints :: [STwo Point Double] -> IO [(ForeignPtr GLfloat, Int)]
flattenPoints [] = return []
flattenPoints ps = do
  let (now, later) = splitAt (chunkBytes `div` (16 * sizeOf (0::GLfloat))) ps
  fptr <- mallocForeignPtrBytes chunkBytes
  withForeignPtr fptr $ \ptr -> do
    count <- pokeArrayCount ptr (flatten now)
    rest <- flattenPoints later
    return $ (fptr, count `div` 16) : rest

chunkBytes :: Int
chunkBytes = 16 * 1024 * 4 * 4 * 4

withPointDraw :: Box -> (String -> IO a) -> (([(ForeignPtr GLfloat, Int)] -> IO Int) -> IO a) -> IO a
withPointDraw (Box lx ly hx hy) cbFail cbOk = do
  withProgram (Just circleVertSource) Nothing (Just circleFragSource) cbFail $ \circle -> do
    glUseProgram circle
    mvp' <- withCString "mvp" $ glGetUniformLocation circle . castPtr
    withArray [ realToFrac $ 2 / (hx - lx), 0, 0, realToFrac $ 2 / (hy - ly) ] $ glUniformMatrix2fv mvp' 1 (fromIntegral GL_FALSE)
    glUseProgram 0
    cbOk $ \ps -> if null ps then return 0 else do
      glClearColor 0 0 0 1
      glClear (fromIntegral GL_COLOR_BUFFER_BIT)
      glEnable GL_BLEND
      glBlendFunc GL_SRC_ALPHA GL_ONE
      glUseProgram circle
      glEnableVertexAttribArray 0
      forM_ ps $ \(fptr, count) -> withForeignPtr fptr $ \ptr -> do
        glVertexAttribPointer 0 4 GL_FLOAT (fromIntegral GL_FALSE) 0 ptr
        glDrawArrays GL_QUADS 0 (4 * fromIntegral count)
      glDisableVertexAttribArray 0
      glUseProgram 0
      return (sum' $ map snd ps)

pokeArrayCount :: Ptr GLfloat -> [GLfloat] -> IO Int
pokeArrayCount ptr vals0 = go vals0 0
  where
    go [] n         = return n
    go (val:vals) n = do pokeElemOff ptr n val; go vals (n + 1)

withProgram :: Maybe String -> Maybe String -> Maybe String -> (String -> IO a) -> (GLuint -> IO a) -> IO a
withProgram mv mg mf cbFail cbOk = do
  prog <- glCreateProgram
  let compile _ Nothing = return ()
      compile t (Just src) = withCString src $ \s -> with s $ \p -> do
        sh <- glCreateShader t
        glShaderSource sh 1 (castPtr p) nullPtr
        glCompileShader sh
        glAttachShader prog sh
        glDeleteShader sh
  compile GL_VERTEX_SHADER   mv
  compile GL_GEOMETRY_SHADER mg
  compile GL_FRAGMENT_SHADER mf
  glLinkProgram prog
  result <- with 0 $ \p -> glGetProgramiv prog GL_LINK_STATUS p >> peek p
  logLen <- with 0 $ \p -> glGetProgramiv prog GL_INFO_LOG_LENGTH p >> peek p
  logTxt <- if logLen <= 1 then return "" else allocaArray (1 + fromIntegral logLen) $ \p -> do
    glGetProgramInfoLog prog (fromIntegral logLen) nullPtr p
    peekCStringLen (castPtr p, fromIntegral logLen - 1)
  r <- if result == fromIntegral GL_TRUE then cbOk prog else cbFail logTxt
  --glDeleteProgram prog
  return r

circleVertSource :: String
circleVertSource = unlines
  [ "#version 330"
  , "uniform mat2 mvp;"
  , "layout (location = 0) in vec4 position;"
  , "smooth out vec3 texCoord;"
  , "void main() {"
  , "  const float minSize = 1.0;"
  , "  const float maxSize = 512.0;"
  , "  const float brightness = 1.0 / 256.0;"
  , "  const float gamma = 1.0;"
  , "  float scaling = length(mvp * vec2(1.0)) / length(vec2(1.0));"
  , "  float q = sqrt(1.0/3.0) * sqrt(position.z) / scaling;"
  , "  vec2 r = mvp * vec2(q);"
  , "  float size = length(r) / scaling;"
  , "  float alpha = pow(brightness, pow(size / maxSize, gamma));"
  , "  float s = max(minSize * scaling, length(r));"
  , "  vec2 delta = sqrt(2.0) * vec2(cos(position.w), sin(position.w));"
  , "  gl_Position = vec4(mvp * (position.xy + s * delta), 0.0, 1.0);"
  , "  texCoord = vec3(delta, alpha);"
  , "}"
  ]

circleFragSource :: String
circleFragSource = unlines
  [ "#version 330"
  , "in vec3 texCoord;"
  , "out vec4 colour;"
  , "void main() {"
  , "  const float lineWidth = 1.0;"
  , "  const vec3 blue   = vec3(0.0, 0.0, 1.0);"
  , "  const vec3 orange = vec3(1.0, 0.7, 0.0);"
  , "  vec2 p = texCoord.xy;"
  , "  vec2 dp = dFdx(p);"
  , "  float r = length(p);"
  , "  float t = texCoord.z;"
  , "  if (r >= 1.0) {"
  , "    discard;"
  , "  } else if (r >= 1.0 - lineWidth * length(dp)) {"
  , "    colour = vec4(vec3(mix(0.0, 1.0, t)), t);"
  , "  } else {"
  , "    discard;"
  , "  }"
  , "}"
  ]
