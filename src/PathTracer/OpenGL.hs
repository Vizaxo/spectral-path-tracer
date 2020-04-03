module PathTracer.OpenGL where

import Control.Concurrent.STM
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Data.Time.Clock
import Foreign.Ptr
import Foreign.Storable
import Graphics.GLUtil
import Graphics.Rendering.OpenGL (($=))
import System.Exit
import System.IO
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW

import PathTracer.STMState
import PathTracer.Types

screenRect :: [GL.Vector2 Float]
screenRect = [ GL.Vector2 (-1.0) (-1.0)
           , GL.Vector2 1.0 (-1.0)
           , GL.Vector2 1.0 1.0
           , GL.Vector2 1.0 1.0
           , GL.Vector2 (-1.0) 1.0
           , GL.Vector2 (-1.0) (-1.0)
           ]

makeWindow :: MonadIO m => m ()
makeWindow = liftIO $ do
  GLFW.initialize
  GLFW.openWindowHint GLFW.OpenGLVersionMajor 3
  GLFW.openWindowHint GLFW.OpenGLVersionMinor 3
  GLFW.openWindowHint GLFW.OpenGLProfile GLFW.OpenGLCoreProfile
  GLFW.openWindow (GL.Size 400 600) [] GLFW.Window
  GLFW.windowTitle $= "PathTracer HS"
  GLFW.windowCloseCallback $= exitSuccess

  -- Disable vsync
  GLFW.swapInterval $= 0

initOGL :: MonadIO m => m ()
initOGL = do
  GL.debugOutput $= GL.Enabled
  GL.polygonMode $= (GL.Fill, GL.Fill)
  GL.cullFace $= Nothing
  GL.depthFunc $= Just GL.Less
  GL.clearColor $= GL.Color4 0.0 0.0 0.0 0.0

initRenderState :: MonadIO m => FilePath -> m RenderState
initRenderState shaderDir = do
  shaderProg <- compileShaders shaderDir >>= \case
    Just sp -> pure sp
    Nothing -> error "Shader compilation failed"

  windowSize <- GL.get GLFW.windowSize

  vao <- liftIO $ makeVAO $ do
      let pos = GL.VertexArrayDescriptor 2 GL.Float
            (fromIntegral $ sizeOf (undefined :: GL.Vector2 Float)) offset0
          posAttribute  = getAttrib shaderProg "pos"
      vbo <- makeBuffer GL.ArrayBuffer screenRect
      GL.vertexAttribArray posAttribute $= GL.Enabled
      GL.vertexAttribPointer posAttribute $= (GL.ToFloat, pos)
  GL.bindVertexArrayObject $= Just vao

  texture0 <- liftIO $ readTexture (shaderDir ++ "/grey-noise-32f-1024-1024.tiff") >>= \case
    Left e -> error $ "Failed to load texture: " ++ e
    Right t -> pure t

  buffers <- initBuffers windowSize

  currentTime <- liftIO getCurrentTime
  pure (RenderState shaderProg vao False shaderDir
        windowSize currentTime
        buffers texture0 1 False
        (GL.Position 0 0) True)

postInit :: MonadIO m => TVar RenderState -> m ()
postInit s = do
  GLFW.windowSizeCallback $= updateWindowSize s
  GLFW.keyCallback $= keyCallback s

keyCallback :: TVar RenderState -> GLFW.Key -> GLFW.KeyButtonState -> IO ()
keyCallback _ (GLFW.SpecialKey GLFW.ESC) GLFW.Press
  = exitSuccess
keyCallback s (GLFW.CharKey 'R') GLFW.Press
  = void $ runSTMStateT s $ modify (set shouldReset True)
keyCallback _ _ _ = pure ()

updateWindowSize :: MonadIO m => TVar RenderState -> GL.Size -> m ()
updateWindowSize s size = void $ runSTMStateT s $
  modify (set shouldReset True . set windowSize size)

initBuffers :: MonadIO m => GL.Size -> m [RenderBuffer]
initBuffers size = do
  buffers <- replicateM 2 (genRenderBuffer size)
  mapM_ clearBuffer buffers
  pure buffers

genTextureFloat :: MonadIO m => GL.GLint -> GL.GLint -> m GL.TextureObject
genTextureFloat width height = do
  t <- liftIO GL.genObjectName
  GL.textureBinding GL.Texture2D $= Just t
  liftIO $ GL.texImage2D GL.Texture2D GL.NoProxy
    0 GL.RGBA32F (GL.TextureSize2D width height)
    0 (GL.PixelData GL.RGBA GL.Float (nullPtr :: Ptr Float))
  pure t

genRenderBuffer :: MonadIO m => GL.Size -> m RenderBuffer
genRenderBuffer (GL.Size width height) = do
  texture0 <- genTextureFloat width height
  texture1 <- genTextureFloat width height
  fbo <- GL.genObjectName
  pure (RenderBuffer texture0 texture1 A fbo)


clearBuffer :: MonadIO m => RenderBuffer -> m ()
clearBuffer b = do
  GL.bindFramebuffer GL.Framebuffer $= (b^.fbo)
  GL.activeTexture $= GL.TextureUnit 0
  GL.textureFilter GL.Texture2D $= ((GL.Nearest, Nothing), GL.Nearest)
  texture2DWrap $= (GL.Repeated, GL.Repeat)
  GL.textureBinding GL.Texture2D $= Just (b^.textureA)
  liftIO $ GL.framebufferTexture2D GL.Framebuffer
    (GL.ColorAttachment 0) GL.Texture2D (b^.textureA) 0
  liftIO $ GL.clear [GL.ColorBuffer]

  GL.textureBinding GL.Texture2D $= Just (b^.textureB)
  liftIO $ GL.framebufferTexture2D GL.Framebuffer
    (GL.ColorAttachment 0) GL.Texture2D (b^.textureB) 0
  liftIO $ GL.clear [GL.ColorBuffer]

makeShaderProgram :: FilePath -> IO ShaderProgram
makeShaderProgram shaderDir = loadShaderProgram
  [ (GL.VertexShader, shaderDir ++ "/vertex.glsl")
  , (GL.FragmentShader, shaderDir ++ "/fragment.glsl")
  ]

recompileIfDirty :: (MonadState RenderState m, MonadIO m) => m ()
recompileIfDirty = do
  rs <- get
  when (rs^.dirty) $ do
    compileShaders (rs^.shaderDir) >>= \case
      Nothing -> modify (set dirty False)
      Just sp -> modify (set dirty False . set shaderProg sp)

compileShaders :: MonadIO m => FilePath -> m (Maybe ShaderProgram)
compileShaders shaderDir = do
  liftIO (try (makeShaderProgram shaderDir)) >>= \case
    Right sp -> do
      GL.currentProgram $= Just (program sp)
      liftIO (putStrLn " Recompiled\n")
      pure (Just sp)
    Left e -> do
      liftIO (print (e :: IOException))
      liftIO (putStrLn " Shader compilation failed\n")
      pure Nothing

uniformExists :: GL.UniformLocation -> Bool
uniformExists (GL.UniformLocation (-1)) = False
uniformExists _ = True

-- Set a uniform without giving a warning or error if it is not active
safeSetUniform :: (GL.Uniform a, MonadGet RenderState m, MonadIO m)
  => String -> a -> m ()
safeSetUniform name v = do
  rs <- get
  uLocation <- GL.get (GL.uniformLocation (rs^.shaderProg&program) name)
  when (uniformExists uLocation) $ GL.uniform uLocation $= v

bindTexture :: (MonadGet RenderState m, MonadIO m)
  => GL.TextureObject -> String -> GL.TextureUnit -> m ()
bindTexture texture uniformName textureUnit = do
  GL.activeTexture $= textureUnit
  GL.textureBinding GL.Texture2D $= Just texture
  GL.textureFilter GL.Texture2D $= ((GL.Nearest, Nothing), GL.Nearest)
  texture2DWrap $= (GL.Repeated, GL.Repeat)
  safeSetUniform uniformName textureUnit

renderToBuffer :: (MonadGet RenderState m, MonadIO m)
  => RenderBuffer -> Integer -> m RenderBuffer
renderToBuffer b id = do
  rs <- get
  let t = getPreviousTexture b
  when (rs^.shouldClearBuffers) (clearBuffer b)
  GL.bindFramebuffer GL.Framebuffer $= (b^.fbo)
  GL.activeTexture $= GL.TextureUnit 0
  GL.textureFilter GL.Texture2D $= ((GL.Nearest, Nothing), GL.Nearest)
  GL.textureBinding GL.Texture2D $= Just t
  liftIO $ GL.framebufferTexture2D GL.Framebuffer
    (GL.ColorAttachment 0) GL.Texture2D t 0

  texture2DWrap $= (GL.Repeated, GL.Repeat)
  safeSetUniform "bufferId" (fromInteger id :: GL.GLint)
  liftIO $ GL.drawArrays GL.Triangles 0 (fromIntegral (length screenRect))

  pure (over lastWritten swapLastWritten b)

renderToScreen :: (MonadGet RenderState m, MonadIO m) => Float -> m ()
renderToScreen id = do
  GL.bindFramebuffer GL.Framebuffer $= GL.defaultFramebufferObject
  safeSetUniform "bufferId" ((-1) :: GL.GLint)
  liftIO $ GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  liftIO $ GL.drawArrays GL.Triangles 0 (fromIntegral (length screenRect))
  liftIO $ GLFW.swapBuffers

bindBufferTexture :: (MonadGet RenderState m, MonadIO m)
  => RenderBuffer -> Integer -> m ()
bindBufferTexture b id =
  let t = getLastWrittenTexture b
  in bindTexture t ("buffer" ++ show id) (GL.TextureUnit (2+fromInteger id))

deleteBuffer :: MonadIO m => RenderBuffer -> m ()
deleteBuffer b = do
  GL.deleteObjectName (b^.textureA)
  GL.deleteObjectName (b^.textureB)
  GL.deleteObjectName (b^.fbo)

reset :: (MonadState RenderState m, MonadIO m) => m ()
reset = do
  rs <- get
  mapM_ deleteBuffer (rs^.buffers)
  GL.viewport $= ((GL.Position 0 0), rs^.windowSize)
  bs <- initBuffers (rs^.windowSize)
  modify (set shouldReset False . set frameNo 1 . set buffers bs)

renderFrame :: (MonadState RenderState m, MonadIO m) => Float -> UTCTime -> m ()
renderFrame iTime t = do
  GL.get GL.errors >>= \case
    [] -> pure ()
    es -> liftIO $ print es

  recompileIfDirty

  rs <- get
  mouse@(GL.Position mouseX mouseY) <- GL.get GLFW.mousePos
  modify (set mousePos mouse)
  when (rs^.shouldReset) reset
  when (rs^.mousePos /= mouse) (modify (set shouldClearBuffers True . set frameNo 1))
  rs <- get

  let tPrev = rs^.lastRenderTime
      dt = realToFrac (diffUTCTime t tPrev) :: Float
      fps = 1.0 / dt
  modify (set lastRenderTime t)

  -- Set uniforms
  safeSetUniform "iTime" iTime
  safeSetUniform "iFrame" (fromIntegral (rs^.frameNo) :: GL.GLint)
  modify (over frameNo (+1))
  safeSetUniform "iDeltaTime" dt
  let (GL.Size width height) = rs^.windowSize
  safeSetUniform "iResolution"
    (GL.Vector2 (fromIntegral width) (fromIntegral height) :: GL.Vector2 Float)
  safeSetUniform "iMousePos" (GL.Vector2 @Float (fromIntegral mouseX)
                              (fromIntegral (height - mouseY)))
  bindTexture (rs^.texture0) "texture0" (GL.TextureUnit 1)

  let numberedBuffers = zip (rs^.buffers) [0..]
  mapM_ (uncurry bindBufferTexture) numberedBuffers
  bs' <- traverse (uncurry renderToBuffer) numberedBuffers
  modify (set buffers bs')
  rs <- get
  let numberedBuffers = zip (rs^.buffers) [0..]
  mapM_ (uncurry bindBufferTexture) numberedBuffers
  when (rs^.shouldClearBuffers) (modify (set shouldClearBuffers False))

  renderToScreen (-1)

  liftIO $ putStr $ "FPS: " ++ show fps ++ "fps \t"
    ++ "Window size: " ++ show width ++ "*" ++ show height ++ "\r"
  liftIO $ hFlush stdout
