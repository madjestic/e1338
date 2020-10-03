{-# LANGUAGE TemplateHaskell #-}
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
  , bindTexureUniforms
  , render
  , Backend (..)
  , BackendOptions (..)
  ) where

import Control.Monad
import Control.Concurrent
import Data.Text                              (Text)
import Data.List.Split                        (splitOn)
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
import Project                 (Project)
import Texture                 (path)

import Linear.Vector
import Utils                   ()

import Data.Foldable     as DF (toList)
import Linear.Projection as LP (perspective)

import Unsafe.Coerce

import Control.Lens       hiding (transform, indexed)
import Debug.Trace as DT

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
       primitiveMode :: PrimitiveMode -- Triangles | Points
     } deriving Show

data Drawable
  =  Drawable
     { _uniforms   :: Uniforms
     , _descriptor :: Descriptor
     , _program    :: Program
     } deriving Show

data Uniforms
  =  Uniforms
     {
       _u_mats  :: Material 
     , _u_prog  :: Program
     , _u_mouse :: (Double, Double)
     , _u_time  :: Float
     , _u_res   :: (CInt, CInt)
     --, u_proj  :: M44 Double --GLmatrix GLfloat
     , _u_cam   :: M44 Double 
     , _u_xform :: M44 Double 
     } deriving Show

$(makeLenses ''Drawable)
$(makeLenses ''Uniforms)

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

(<$.>) :: (a -> b) -> [a] -> [b]
(<$.>) = map

(<*.>) :: [a -> b] -> [a] -> [b]
(<*.>) = zipWith ($)

fromGame :: Game -> [Object] -> Float -> [Drawable]
fromGame game objs time = drs -- (drs, drs')
  where
    -- objsDrs = (view objects game) -- :: [Object]
    mpos = unsafeCoerce $ view (camera . controller . device' . mouse . pos) game -- :: (Double, Double)
    resX = fromEnum $ view (options . resx) game :: Int
    resY = fromEnum $ view (options . resy) game :: Int
    res  = ((toEnum resX), (toEnum resY)) :: (CInt, CInt)
    cam  = view (camera . controller . Controllable.transform) game :: M44 Double
    drs  = concat $ fmap (fromObject mpos time res cam) objs :: [Drawable]
    --drs  = concat $ fmap (fromObject mpos time res cam) (DT.trace ("objs :" ++ show objs) $ objs) :: [Drawable]
              
fromObject :: (Double, Double) -> Float -> (CInt, CInt) -> M44 Double -> Object -> [Drawable]
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
    let currentTime = fromInteger (unsafeCoerce ticks :: Integer) :: Float
        fntObjs = concat $ toListOf (objects . gui . fonts) game :: [Object]
        fgrObjs = concat $ toListOf (objects . foreground)  game :: [Object]
        
        --fntsDrs = fromGame game (DT.trace ("fntObjs :" ++ show fntObjs) $ fntObjs) currentTime :: [Drawable]
        fntsDrs = fromGame game fntObjs currentTime :: [Drawable]
        objsDrs = fromGame game fgrObjs currentTime :: [Drawable]

        --texPaths = concat $ toListOf (foreground . traverse . materials . traverse . Material.textures) (view objects game) :: [FilePath]
        texPaths = concat $ toListOf ( traverse . materials . traverse . Material.textures) (fgrObjs ++ fntObjs) :: [FilePath]
        fps  = show (unsafeCoerce ticks :: Int)

    _ <- mapM_ (draw texPaths (opts { primitiveMode = Triangles }) window) objsDrs
        
    currentTime <- SDL.time                          
    dt <- (currentTime -) <$> readMVar lastInteraction -- swapMVar lastInteraction currentTime --dtime
    -- putStrLn $ "FPS :" ++ show (1/dt) ++ "\n"
    _ <- drawString (draw texPaths opts window) fntsDrs $ show $ round (1/dt) -- render FPS
    
    SDL.glSwapWindow window
    
render _ Vulkan _ _ _ = undefined

-- | given a string of drawables, return a formatted string (e.g. add offsets for drawable chars)
format :: [Drawable] -> [Drawable]
format drs = drw
  where
    drw = fmap offsetChar (zip drs [0..])

offsetChar :: (Drawable, Int) -> Drawable
offsetChar (drw, offset) = drw'
  where
    uns  = view uniforms drw
    rot0 = view _m33 (view (uniforms . u_xform) drw)
    tr0  = view translation (view (uniforms . u_xform) drw)
    s    = 0.07 -- scale
    offsetM44 = mkTransformationMat rot0 (tr0 ^+^ (V3 (-0.105 + fromIntegral offset*s) 0 0))
    drw' = set (uniforms . u_xform) offsetM44 drw
    
-- | Alphabet of drawables -> String -> String of drawables
drawableString :: [Drawable] -> String -> [Drawable]
drawableString drs str = drws
  where
    drws = fmap (drawableChar drs) str

-- | Alphabet of drawables -> Char -> a drawable char
drawableChar :: [Drawable] -> Char -> Drawable
drawableChar drs chr =
  case chr of
    '0' -> drs!!0
    '1' -> drs!!1
    '2' -> drs!!2
    '3' -> drs!!3
    '4' -> drs!!4
    '5' -> drs!!5
    '6' -> drs!!6
    '7' -> drs!!7
    '8' -> drs!!8
    '9' -> drs!!9
    _   -> drs!!0

-- -- | Alphabet of drawables -> a drawable character with an offset
-- formatCharacter :: [Drawable] -> Char -> Int -> Drawable
-- formatCharacter = undefined

drawString :: (Drawable -> IO ()) -> [Drawable] -> String -> IO ()
drawString cmds fntsDrs str =
  do
    mapM_ cmds $ format $ drawableString fntsDrs str

-- drawChar :: (Drawable -> IO ()) -> [Drawable] -> Char -> IO ()
-- drawChar cmds fntsDrs chr = 
  -- do
  --   cmds fntsDrs
    

draw :: [FilePath] -> BackendOptions -> SDL.Window -> Drawable -> IO ()
draw
  texPaths
  opts
  window
  (Drawable
    unis
    (Descriptor vao' numIndices')
    prog) =
  do
    initUniforms texPaths unis
    
    bindVertexArrayObject $= Just vao'
    drawElements (primitiveMode opts) numIndices' GL.UnsignedInt nullPtr
    
    GL.pointSize $= 10

    cullFace  $= Just Back
    depthFunc $= Just Less

bindTexureUniforms :: [Object] -> IO ()
bindTexureUniforms objsDrs =
  do
    print "Loading Textures..."
    _ <- mapM bindTexture $ zip ids txs
    print "Finished loading textures."
      where
        txs = concat $ concat $ fmap (toListOf (materials . traverse . Material.textures)) objsDrs
        ids = take (length txs) [0..]

bindTexture :: (GLuint, FilePath) -> IO ()
bindTexture (txid, tx) =
  do
    texture Texture2D        $= Enabled
    activeTexture            $= TextureUnit txid
    tx0 <-loadTex tx
    textureBinding Texture2D $= Just tx0

initUniforms :: [FilePath] -> Uniforms -> IO ()
initUniforms
  texPaths
  (Uniforms u_mat' u_prog' u_mouse' u_time' u_res' u_cam' u_xform') =
  
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

    -- | Allocate Textures
    -- _ <- DT.trace "suka" $ return ()
    let texNames = fmap getTexName texPaths
    _ <- mapM (allocateTextures program) $ zip texNames [0..]
    
    -- | Unload buffers
    --bindVertexArrayObject         $= Nothing
    --bindBuffer ElementArrayBuffer $= Nothing

    return ()

getTexName :: FilePath -> String
getTexName f = (splitOn "." $ (splitOn "/" f)!!1)!!0

allocateTextures :: Program -> (String, GLuint) -> IO ()
allocateTextures program (tx, txU) =
  do
    -- _ <- DT.trace ("tx :" ++ show tx) $ return ()
    -- _ <- DT.trace ("txU :" ++ show txU) $ return ()
    location <- get (uniformLocation program tx)
    uniform location $= (TextureUnit txU)
    
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

