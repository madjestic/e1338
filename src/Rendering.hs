{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP    #-}

module Rendering
  ( openWindow
  , closeWindow
  , draw
  , initVAO
  , initUniforms
  , initGlobalUniforms
  , render
  , Backend (..)
  , BackendOptions (..)
  ) where

import Control.Monad
import Control.Concurrent
import Data.Text                              (Text)
import Foreign.C
import Foreign.Marshal.Array                  (withArray)
import Foreign.Ptr                            (plusPtr, nullPtr)
import Foreign.Storable                       (sizeOf)
import Graphics.Rendering.OpenGL as GL hiding (color, normal, Size)
import SDL                             hiding (Point, Event, Timer, (^+^), (*^), (^-^), dot, project)
import Graphics.GLUtil                        (readTexture, texture2DWrap)

import LoadShaders
import Game
import Object
import Camera    as C
import Controllable
import Descriptor
import Material
import Mouse
import Project                 (Project, textures)
import Texture                 (path)

import Data.Foldable     as DF (toList)
import Linear.Projection as LP (perspective)

import Unsafe.Coerce

import Control.Lens       hiding (transform, indexed)
-- import Debug.Trace as DT

#ifdef DEBUG
debug = True
#else
debug = False
#endif

data Backend
  = OpenGL
  | Vulkan

data BackendOptions
  =  BackendOptions
     {
       primitiveMode :: PrimitiveMode
     } deriving Show

openWindow :: Text -> (CInt, CInt) -> IO SDL.Window
openWindow title (sizex,sizey) =
  do
    SDL.initialize [SDL.InitVideo]
    SDL.HintRenderScaleQuality $= SDL.ScaleLinear                    
    do renderQuality <- SDL.get SDL.HintRenderScaleQuality          
       when (renderQuality /= SDL.ScaleLinear) $                    
         putStrLn "Warning: Linear texture filtering not enabled!"
         
    let config = OpenGLConfig { glColorPrecision = V4 8 8 8 0
                              , glDepthPrecision = 24
                              , glStencilPrecision = 8
                              , glMultisampleSamples = 8
                              , glProfile = Compatibility Normal 2 1
                              --, glProfile = Core Normal 4 5
                              }

    depthFunc $= Just Less

    window <- SDL.createWindow
              title
              SDL.defaultWindow { SDL.windowInitialSize = V2 sizex sizey
                                , SDL.windowGraphicsContext = OpenGLContext config
                                }

    SDL.showWindow window
    _ <- SDL.glCreateContext window
    
    return window

closeWindow :: SDL.Window -> IO ()
closeWindow window =
  do
    SDL.destroyWindow window
    SDL.quit

-- -- < OpenGL > -------------------------------------------------------------

data Uniforms
  =  Uniforms
     {
       u_mats  :: Material 
     , u_prog  :: Program
     , u_mouse :: (Double, Double)
     , u_time  :: Float
     , u_res   :: (CInt, CInt)
     --, u_proj  :: M44 Double --GLmatrix GLfloat
     , u_cam   :: M44 Double 
     , u_xform :: M44 Double 
     } deriving Show

data Drawable
  =  Drawable
     { uniforms   :: Uniforms
     , descriptor :: Descriptor
     , program    :: Program
     } deriving Show

(<$.>) :: (a -> b) -> [a] -> [b]
(<$.>) = map

(<*.>) :: [a -> b] -> [a] -> [b]
(<*.>) = zipWith ($)

fromGame :: Game -> Float -> [Drawable]
fromGame game time = drs
  where
    objs = (view objects game) :: [Object]
    mpos = unsafeCoerce $ view (camera . controller . device' . mouse . pos) game -- :: (Double, Double)
    resX = fromEnum $ view (options . resx) game :: Int
    resY = fromEnum $ view (options . resy) game :: Int
    res  = ((toEnum resX), (toEnum resY)) :: (CInt, CInt)
    cam  = view (camera . controller . Controllable.transform) game :: M44 Double
    drs  = concat $ fmap (fromObject mpos time res cam) objs :: [Drawable]

fromObject :: (Double, Double) -> Float -> (CInt, CInt) -> M44 Double -> Object -> [Drawable]
--fromObject mpos time res cam obj = (DT.trace ("drs :" ++ show drs) $  drs)
fromObject mpos time res cam obj = drs
  where
    drs      = --undefined :: [Drawable]
      (\u_mats' u_prog' u_mouse' u_time' u_res' u_cam' u_xform' ds' ps'
        -> (Drawable (Uniforms u_mats' u_prog' u_mouse' u_time' u_res' u_cam' u_xform') ds' ps'))
      <$.> mats <*.> progs <*.> mpos_ <*.> time_ <*.> res_ <*.> cam_ <*.> xforms <*.> ds <*.> progs

    n      = length $ view descriptors obj:: Int
    mpos_  = replicate n $ mpos :: [(Double, Double)]
    time_  = replicate n $ time :: [Float]
    res_   = replicate n $ res  :: [(CInt, CInt)]
    cam_   = replicate n $ cam  :: [M44 Double]

    mats   = view Object.materials   obj :: [Material]
    progs  = view Object.programs    obj :: [Program]
    xforms = concat $ replicate n $ view Object.transforms obj :: [M44 Double]
    ds     = view Object.descriptors obj :: [Descriptor]

render :: (MVar Double) -> Backend -> BackendOptions -> SDL.Window -> Game -> IO ()
render lastInteraction Rendering.OpenGL opts window game =
  do
    GL.clearColor $= Color4 0.0 0.0 0.0 1.0
    GL.clear [ColorBuffer, DepthBuffer]

    ticks   <- SDL.ticks
    -- dfps    <- drawString "0"
    let currentTime = fromInteger (unsafeCoerce ticks :: Integer) :: Float
        drs  = fromGame game currentTime :: [Drawable]
        -- fps  = ticks
        --dfps = drawString fps :: [Drawable]

    --mapM_ (draw opts window) (drs ++ fps)
    mapM_ (draw opts window) (drs)

    -- currentTime <- SDL.time                          
    -- dt <- (currentTime -) <$> readMVar lastInteraction -- swapMVar lastInteraction currentTime --dtime
    -- putStrLn $ "FPS :" ++ show (1/dt) ++ "\n"
    SDL.glSwapWindow window
    
render _ Vulkan _ _ _ = undefined

-- drawString :: String -> IO [Drawable]
-- drawString s =
--   do
--     ds <- mapM drawChar s
--     return ds

-- drawChar :: Char -> IO Drawable
-- drawChar = undefined

draw :: BackendOptions -> SDL.Window -> Drawable -> IO ()
draw opts window (Drawable
                   unis
                   (Descriptor vao' numIndices')
                   prog) =
  do
    initUniforms unis
    
    bindVertexArrayObject $= Just vao'
    drawElements (primitiveMode opts) numIndices' GL.UnsignedInt nullPtr
    
    GL.pointSize $= 10

    cullFace  $= Just Back
    depthFunc $= Just Less

initGlobalUniforms :: Project -> IO ()
initGlobalUniforms project =
  do
    print "Loading Textures..."
    _ <- mapM bindTexture $ zip ids txs
    print "Finished loading textures."
      where
        txs = toListOf (textures . traverse . path) project
        ids = take (length txs) [0..]

bindTexture :: (GLuint, FilePath) -> IO ()
bindTexture (txid, tx) =
  do
    texture Texture2D        $= Enabled
    activeTexture            $= TextureUnit txid
    tx0 <-loadTex tx
    textureBinding Texture2D $= Just tx0

initUniforms :: Uniforms -> IO ()
initUniforms (Uniforms u_mat' u_prog' u_mouse' u_time' u_res' u_cam' u_xform') = 
  do
    -- | Shaders
    -- _ <- DT.trace ("vertShader: " ++ show (_vertShader u_mat')) $ return ()
    -- _ <- DT.trace ("vertShader: " ++ show (_fragShader u_mat')) $ return ()

    programDebug <- loadShaders
               [ ShaderInfo VertexShader   (FileSource (_vertShader u_mat' ))
               , ShaderInfo FragmentShader (FileSource (_fragShader u_mat' )) ]
    -- programDebug <- loadShaders
    --            [ ShaderInfo VertexShader   (FileSource ("./mat/constant/src/shader.vert"))
    --            , ShaderInfo FragmentShader (FileSource ("./mat/constant/src/shader.frag")) ]
    
    let program = case debug of
                    True  -> programDebug
                    False -> u_prog'
    currentProgram $= Just program

    let u_mouse       = Vector2 (realToFrac $ fst u_mouse') (realToFrac $ snd u_mouse') :: Vector2 GLfloat
    location0         <- get (uniformLocation program "u_mouse'")
    uniform location0 $= u_mouse

    let resX          = fromIntegral $ fromEnum $ fst u_res' :: Float
        resY          = fromIntegral $ fromEnum $ snd u_res' :: Float
        u_res         = Vector2 resX resY :: Vector2 GLfloat
    location1         <- get (uniformLocation program "u_resolution")
    uniform location1 $= u_res

    location2         <- get (uniformLocation program "u_time'")
    uniform location2 $= (u_time' :: GLfloat)

    let proj =          
          fmap realToFrac . concat $ fmap DF.toList . DF.toList -- convert to GLfloat
          --               FOV    Aspect      Near   Far
          $ LP.perspective (pi/2) (resX/resY) (0.01) 1.0 :: [GLfloat]

    persp             <- GL.newMatrix RowMajor proj :: IO (GLmatrix GLfloat)
    location3         <- get (uniformLocation program "persp")
    uniform location3 $= persp

    let cam =
          fmap realToFrac . concat $ fmap DF.toList . DF.toList $ u_cam'
    camera            <- GL.newMatrix RowMajor cam :: IO (GLmatrix GLfloat)
    location4         <- get (uniformLocation program "camera")
    uniform location4 $= camera

    let mtx =
          fmap realToFrac . concat $ fmap DF.toList . DF.toList $ u_xform'
    transform         <- GL.newMatrix RowMajor mtx :: IO (GLmatrix GLfloat)
    location5         <- get (uniformLocation program "transform")
    uniform location5 $= transform --u_xform'

    location6 <- get (uniformLocation program "tex_00")
    uniform location6 $= (TextureUnit 0)
    location7 <- get (uniformLocation program "tex_01")
    uniform location7 $= (TextureUnit 1)
    
    -- | Unload buffers
    --bindVertexArrayObject         $= Nothing
    --bindBuffer ElementArrayBuffer $= Nothing

    return ()      

       -- | Indices -> Stride -> ListOfFloats -> Material -> Descriptor
initVAO :: ([Int], Int, [Float], Material) -> IO Descriptor
initVAO (idx', st', vs', matPath) =
  do
    let
      idx = (fmap unsafeCoerce idx') :: [GLuint]
      vs  = (fmap unsafeCoerce vs')  :: [GLfloat]
    --_ <- DT.trace ("Rendering.initVAO.vs :" ++ show vs) $ return ()
    -- | VAO
    vao <- genObjectName
    bindVertexArrayObject $= Just vao 
    -- | VBO
    vertexBuffer <- genObjectName
    bindBuffer ArrayBuffer $= Just vertexBuffer
    withArray vs $ \ptr ->
      do
        let sizev = fromIntegral ((length vs) * sizeOf (head vs))
        bufferData ArrayBuffer $= (sizev, ptr, StaticDraw)
    -- | EBO
    elementBuffer <- genObjectName
    bindBuffer ElementArrayBuffer $= Just elementBuffer
    let numIndices = length (idx)
    withArray (idx) $ \ptr ->
      do
        let indicesSize = fromIntegral (numIndices * sizeOf (head (idx)))
        bufferData ElementArrayBuffer $= (indicesSize, ptr, StaticDraw)
        
        -- | Bind the pointer to the vertex attribute data
        let floatSize  = (fromIntegral $ sizeOf (0.0::GLfloat)) :: GLsizei
            stride     = (fromIntegral st') * floatSize
        
        -- | Alpha
        vertexAttribPointer (AttribLocation 0) $= (ToFloat, VertexArrayDescriptor 1 Float stride ((plusPtr nullPtr . fromIntegral) (0 * floatSize)))
        vertexAttribArray   (AttribLocation 0) $= Enabled
        -- | Colors
        vertexAttribPointer (AttribLocation 1) $= (ToFloat, VertexArrayDescriptor 3 Float stride ((plusPtr nullPtr . fromIntegral) (1 * floatSize)))
        vertexAttribArray   (AttribLocation 1) $= Enabled
        -- | Normals
        vertexAttribPointer (AttribLocation 2) $= (ToFloat, VertexArrayDescriptor 3 Float stride ((plusPtr nullPtr . fromIntegral) (4 * floatSize)))
        vertexAttribArray   (AttribLocation 2) $= Enabled
        -- | UVW
        vertexAttribPointer (AttribLocation 3) $= (ToFloat, VertexArrayDescriptor 3 Float stride ((plusPtr nullPtr . fromIntegral) (7 * floatSize)))
        vertexAttribArray   (AttribLocation 3) $= Enabled
        -- | Positions
        vertexAttribPointer (AttribLocation 4) $= (ToFloat, VertexArrayDescriptor 3 Float stride ((plusPtr nullPtr . fromIntegral) (10 * floatSize)))
        vertexAttribArray   (AttribLocation 4) $= Enabled

    return $ Descriptor vao (fromIntegral numIndices)

--f = "textures/8192_moon.jpg"

loadTex :: FilePath -> IO TextureObject
loadTex f =
  do
    t <- either error id <$> readTexture f
    texture2DWrap $= (Repeated, ClampToEdge)
    --textureFilter Texture2D $= ((Linear', Nothing), Linear')
    textureFilter  Texture2D $= ((Linear', Just Nearest), Linear')
    blend $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    generateMipmap' Texture2D
    return t

