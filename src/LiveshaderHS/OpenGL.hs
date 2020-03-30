module LiveshaderHS.OpenGL where

import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Data.Time.Clock
import Foreign.Storable
import Graphics.GLUtil
import Graphics.Rendering.OpenGL (($=))
import System.Exit
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW

import LiveshaderHS.STMState
import LiveshaderHS.Types

makeWindow :: IO ()
makeWindow = do
  GLFW.initialize
  GLFW.openWindowHint GLFW.OpenGLVersionMajor 3
  GLFW.openWindowHint GLFW.OpenGLVersionMinor 3
  GLFW.openWindowHint GLFW.OpenGLProfile GLFW.OpenGLCoreProfile
  GLFW.openWindow (GL.Size 400 600) [] GLFW.Window
  GLFW.windowTitle $= "Liveshader HS"
  GLFW.windowCloseCallback $= exitSuccess
  GLFW.keyCallback $= keyCallback

  -- Disable vsync
  GLFW.swapInterval $= 0

keyCallback :: GLFW.Key -> GLFW.KeyButtonState -> IO ()
keyCallback (GLFW.SpecialKey GLFW.ESC) GLFW.Press = exitSuccess
keyCallback _ _ = pure ()

initOGL :: FilePath -> IO RenderState
initOGL shaderDir = do
  GL.debugOutput $= GL.Enabled
  GL.polygonMode $= (GL.Fill, GL.Fill)
  GL.cullFace $= Nothing
  GL.depthFunc $= Just GL.Less

  vao <- GL.genObjectName
  GL.bindVertexArrayObject $= Just vao
  GL.clampColor GL.ClampVertexColor $= GL.ClampOff
  GL.clampColor GL.ClampFragmentColor $= GL.ClampOff
  GL.clampColor GL.ClampReadColor $= GL.ClampOff
  GL.clearColor $= GL.Color4 0.0 0.0 0.0 0.0

  shaderProg <- makeShaderProgram shaderDir
  windowSize@(GL.Size width height) <- GL.get GLFW.windowSize

  vao <- makeVAO $ do
      let pos = GL.VertexArrayDescriptor 2 GL.Float (fromIntegral $ sizeOf (undefined :: GL.Vector2 Float)) offset0
          posAttribute  = getAttrib shaderProg "pos"
      vbo <- makeBuffer GL.ArrayBuffer vertices
      GL.vertexAttribArray posAttribute $= GL.Enabled
      GL.vertexAttribPointer posAttribute $= (GL.ToFloat, pos)

  texture <- readTexture (shaderDir ++ "/image.png") >>= \case
    Left e -> error $ "Failed to load texture: " ++ e
    Right t -> pure t

  buffer0 <- liftIO $ freshTextureFloat (fromIntegral width) (fromIntegral height) TexRGBA
  fbo <- GL.genObjectName

  -- Clear buffer0 texture
  GL.bindFramebuffer GL.Framebuffer $= fbo
  GL.activeTexture $= GL.TextureUnit 1
  GL.textureBinding GL.Texture2D $= Just buffer0
  GL.textureFilter GL.Texture2D $= ((GL.Nearest, Nothing), GL.Nearest)
  texture2DWrap $= (GL.Repeated, GL.Repeat)
  liftIO $ GL.framebufferTexture2D GL.Framebuffer (GL.ColorAttachment 0) GL.Texture2D buffer0 0
  liftIO $ GL.clear [GL.ColorBuffer]

  currentTime <- getCurrentTime
  pure (RenderState shaderProg vao False shaderDir windowSize currentTime texture buffer0 fbo)


vertices :: [GL.Vector2 Float]
vertices = [ GL.Vector2 (-1.0) (-1.0)
           , GL.Vector2 1.0 (-1.0)
           , GL.Vector2 1.0 1.0
           , GL.Vector2 1.0 1.0
           , GL.Vector2 (-1.0) 1.0
           , GL.Vector2 (-1.0) (-1.0)
           ]

makeShaderProgram :: FilePath -> IO ShaderProgram
makeShaderProgram shaderDir = loadShaderProgram
  [ (GL.VertexShader, shaderDir ++ "/vertex.glsl")
  , (GL.FragmentShader, shaderDir ++ "/fragment.glsl")
  ]

recompileIfDirty :: (MonadState RenderState m, MonadIO m) => m ()
recompileIfDirty = do
  rs <- get
  when (rs ^. dirty) $ do
    liftIO (putStr "\nRecompiling shaders...")
    liftIO (try (makeShaderProgram (rs ^. shaderDir))) >>= \case
      Right sp -> do
        modify (set shaderProg sp . set dirty False)
        liftIO (putStrLn " Recompiled")
      Left e -> do
        modify (set dirty False)
        liftIO (print (e :: IOException))
        liftIO (putStrLn " Shader compilation failed")

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

renderFrame :: (MonadState RenderState m, MonadIO m) => Float -> UTCTime -> m ()
renderFrame iTime t = do
  GL.get GL.errors >>= \case
    [] -> pure ()
    es -> liftIO $ print es

  recompileIfDirty

  rs <- get
  let tPrev = rs^.lastRenderTime
      dt = realToFrac (diffUTCTime t tPrev) :: Float
      fps = 1.0 / dt
  liftIO (putStr ("FPS: " ++ show fps ++ "\t"))
  modify (set lastRenderTime t)

  GL.currentProgram $= Just (program (rs ^. shaderProg))
  GL.bindVertexArrayObject $= Just (rs ^. vao)

  -- Set uniforms
  safeSetUniform "iTime" iTime
  safeSetUniform "iDeltaTime" dt
  let (GL.Size width height) = rs^.windowSize
  safeSetUniform "iResolution"
    (GL.Vector2 (fromIntegral width) (fromIntegral height) :: GL.Vector2 Float)
  liftIO (putStr ("Window size: " ++ show width ++ "*" ++ show height ++ "\r"))
  (GL.Position mouseX mouseY) <- GL.get GLFW.mousePos
  safeSetUniform "iMousePos"
    (GL.Vector2 (fromIntegral mouseX) (fromIntegral (height - mouseY)) :: GL.Vector2 Float)

  bindTexture (rs^.texture0) "texture0" (GL.TextureUnit 0)
  bindTexture (rs^.buffer0) "buffer0" (GL.TextureUnit 1)

  -- Render to buffer 0
  GL.bindFramebuffer GL.Framebuffer $= (rs^.buffer0fbo)
  safeSetUniform "isBuffer" (0 :: Float)
  liftIO $ GL.drawArrays GL.Triangles 0 (fromIntegral (length vertices))

  -- Render to screen
  GL.bindFramebuffer GL.Framebuffer $= GL.defaultFramebufferObject
  safeSetUniform "isBuffer" (1 :: Float)
  liftIO $ GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  liftIO $ GL.drawArrays GL.Triangles 0 (fromIntegral (length vertices))
  liftIO $ GLFW.swapBuffers
