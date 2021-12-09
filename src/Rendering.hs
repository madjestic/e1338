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
--  , drawGraph
  , initVAO
  , initUniforms
--  , initResources  
--  , bindTexureUniforms
  , genTexObject
  , bindTexture
  , bindTextureObject
  , render
--  , pixelGrid
  , loadTex
  , Backend (..)
  , BackendOptions (..)
  ) where

import Control.Monad
import Control.Concurrent
import Data.Maybe                             (fromMaybe)
import Data.Text                              (Text)
import Data.UUID
import Data.UUID.V4
import Data.List.Split                        (splitOn)
import Data.List                              (sort)
import Foreign.C
import Foreign.Marshal.Array                  (withArray)
import Foreign.Ptr                            (plusPtr, nullPtr, Ptr(..))
import Foreign.Storable                       (sizeOf)
import Graphics.Rendering.OpenGL as GL hiding (color, normal, Size)
import SDL                             hiding (Point, Event, Timer, (^+^), (*^), (^-^), dot, project, Texture)
import Linear.Vector
import Linear.Matrix
import Data.Foldable     as DF (toList)
import Linear.Projection as LP (perspective, infinitePerspective)
import Unsafe.Coerce
import Control.Lens       hiding (xform, indexed)

import LoadShaders
import App
import Application
import Object            as O
import Camera            as C
import Controllable
import Descriptor
import Graph
import Material          as M
import Mouse
import Project                 (Project)
import Texture           as T
import Utils

import Data.Massiv.Array.Mutable as AM
import Data.Massiv.Array as A                 (getComp, makeArray, withPtr, size, D, S, Ix2 ((:.)), Array, Sz2(..), Sz (Sz2, Sz))
import Data.Massiv.Array.Manifest as AM'      (toByteString)
import Data.Word                              (Word8)
import Graphics.GLUtil.Textures               (loadTexture, texInfo, TexInfo)
import Graphics.GLUtil                        (readTexture, texture2DWrap, TexColor(..))

import Debug.Trace as DT


debug = True





data Backend
  = OpenGL
  | Vulkan

data BackendOptions
  =  BackendOptions
     {
       primitiveMode :: PrimitiveMode -- Triangles | Points
     , bgrColor      :: Color4 GLfloat
     , ptSize        :: Float
     } deriving Show

data Drawable
  =  Drawable
     {  name       :: String
     , _uniforms   :: Uniforms
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
     , _u_cam_a :: Double
     , _u_cam_f :: Double
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
                              --, glProfile = Compatibility Normal 2 1
                              , glProfile = Core Normal 4 5
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

fromGame :: App -> [Object] -> Float -> [Drawable]
fromGame app objs time = drs -- (drs, drs')
  where
    mpos = unsafeCoerce $ view (playCam . controller . device' . mouse . pos) app -- :: (Double, Double)
    resX = fromEnum $ view (options . resx) app :: Int
    resY = fromEnum $ view (options . resy) app :: Int
    res  = (toEnum resX, toEnum resY) :: (CInt, CInt)
    cam  = view playCam app :: Camera
    drs  = concatMap (fromObject mpos time res cam) objs :: [Drawable]

fromObject :: (Double, Double) -> Float -> (CInt, CInt) -> Camera -> Object -> [Drawable]
fromObject mpos time res cam obj = drs
  where
    drs      =
      (\u_mats' u_prog' u_mouse' u_time' u_res' u_cam' u_cam_a' u_cam_f' u_xform' ds' ps' name'
        -> Drawable name' (Uniforms u_mats' u_prog' u_mouse' u_time' u_res' u_cam' u_cam_a' u_cam_f' u_xform') ds' ps')
      <$.> mats <*.> progs <*.> mpos_ <*.> time_ <*.> res_ <*.> cam_ <*.> cam_a_ <*.> cam_f_ <*.> xforms <*.> ds <*.> progs <*.> names

    n      = length $ view descriptors obj:: Int
    mpos_  = replicate n mpos :: [(Double, Double)]
    time_  = replicate n time :: [Float]
    res_   = replicate n res  :: [(CInt, CInt)]
    cam_   = replicate n $ view (controller . Controllable.transform) cam  :: [M44 Double]
    cam_a_ = replicate n $ _apt cam :: [Double]
    cam_f_ = replicate n $ _foc cam :: [Double]

    names  = toListOf (O.materials . traverse . M.name) obj :: [String]
    mats   = view O.materials   obj :: [Material]
    progs  = view O.programs    obj :: [Program]
    xforms = concat $ replicate n $ view O.transforms obj :: [M44 Double]
    ds     = view O.descriptors obj :: [Descriptor]

render :: MVar Double
       -> Backend -> BackendOptions
       -> SDL.Window
       -> Application
       -> IO ()
render lastInteraction Rendering.OpenGL opts window application =
  do
    let app = (fromApplication application)

    GL.clearColor $= bgrColor opts --Color4 0.0 0.0 0.0 1.0
    GL.clear [ColorBuffer, DepthBuffer]

    ticks   <- SDL.ticks
    let currentTime = fromInteger (unsafeCoerce ticks :: Integer) :: Float

        fntObjs = concat $ toListOf (objects . gui . fonts) app :: [Object]
        fgrObjs = concat $ toListOf (objects . foreground)  app :: [Object]
        bgrObjs = concat $ toListOf (objects . background)  app :: [Object]

        fntsDrs = fromGame app fntObjs currentTime :: [Drawable]
        objsDrs = fromGame app fgrObjs currentTime :: [Drawable]
        bgrsDrs = fromGame app bgrObjs currentTime :: [Drawable]

        txs     = concat $ toListOf ( traverse . materials . traverse . textures) (fgrObjs ++ fntObjs) :: [Texture]
        hmap    = _hmap application

    mapM_ (draw txs hmap (opts { primitiveMode = Triangles }) window) objsDrs
    mapM_ (draw txs hmap (opts { primitiveMode = Points }) window) bgrsDrs

-- | render FPS
    currentTime <- SDL.time
    dt <- (currentTime -) <$> readMVar lastInteraction
    drawString (draw txs hmap (opts { primitiveMode = Triangles }) window) fntsDrs $ show $ round (1/dt)

    SDL.glSwapWindow window

render _ Vulkan _ _ _ = undefined

-- drawGraph :: Int -> Graph -> IO ()
-- drawGraph s g = do
--   let mArr = view marray g
--       arr  = view array g
--   computeInto mArr $ pixelGrid s arr
--   A.withPtr mArr $ \ptr -> do
--     tex <- genTex (AM.sizeOfMArray mArr) ptr
--     drawPixelGrid tex
--   return ()

-- genTexObject :: Int -> Graph -> IO TextureObject
-- genTexObject s g = do
--   let mArr = view marray g
--       arr  = view array g
--   --computeInto mArr arr-- $ pixelGrid s arr
--   --computeInto mArr $ pixelGrid s arr
--   A.withPtr mArr $ \ptr -> do
--     --genTex (AM.sizeOfMArray mArr) ptr
--     genTex (view sz g) (PixelData Luminance UnsignedByte ptr) --ptr

genTexObject :: Int -> Graph -> IO TextureObject
genTexObject s g = do
  let mArr = view marray g
      arr  = view array g
      arr' = AM'.toByteString arr
      Sz2 resx resy = view sz g
      --txInfo = texInfo 512 512 TexRGBA arr'
      txInfo = texInfo resx resy TexRGBA arr'
  t <- loadTexture txInfo -- :: IO TextureObject
  texture2DWrap $= (Repeated, ClampToEdge)
  textureFilter  Texture2D $= ((Linear', Just Nearest), Linear')
  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  generateMipmap' Texture2D
  return t

-- TODO: draws a square with a default material, using generated texture graph as a texture
-- drawPixelGrid :: TextureObject -> IO ()
-- drawPixelGrid tx = do
--   draw txs hmap opts win drw
--     where
--       txs  = undefined
--       hmap = undefined
--       opts = undefined
--       win  = undefined
--       drw  = undefined

-- pixelGrid :: Int -> Array S Ix2 Word8 -> Array D Ix2 Word8
-- pixelGrid k8 arr = A.makeArray (getComp arr) sz' getNewElt
--   where
--     k = succ k8
--     Sz (n :. m) = size arr
--     sz' = Sz (1 + m * k :. 1 + n * k)
--     getNewElt (j :. i) =
--       if i `mod` k == 0 || j `mod` k == 0
--         then 128
--         else 1 -- (1 - AU.unsafeIndex arr ((i - 1) `div` k :. (j - 1) `div` k)) * 255

-- pixelGrid :: Int -> Array S Ix2 Word8 -> Array D Ix2 Word8
-- pixelGrid k8 arr = A.makeArray (getComp arr) sz' getNewElt
--   where
--     k = succ k8
--     Sz (n :. m) = size arr
--     sz' = Sz (1 + m * k :. 1 + n * k)
--     getNewElt (j :. i) =
--       if i `mod` k == 0 || j `mod` k == 0
--         then 128
--         else 1 -- (1 - AU.unsafeIndex arr ((i - 1) `div` k :. (j - 1) `div` k)) * 255

drawString :: (Drawable -> IO ()) -> [Drawable] -> String -> IO ()
drawString cmds fntsDrs str =
    mapM_ cmds $ format $ drawableString fntsDrs "Hello, World!"--str

-- | given a string of drawables, return a formatted string (e.g. add offsets for drawable chars)
format :: [Drawable] -> [Drawable]
format drs = drw
  where
    drw = fmap formatting (zip drs [0..])

formatting :: (Drawable, Int) -> Drawable
formatting (drw, offset) = drw'
  where
    uns  = view uniforms drw
    rot0 = view _m33 (view (uniforms . u_xform) drw)
    tr0  = view translation (view (uniforms . u_xform) drw)
    s1    = 0.085   -- scale Offset
    s2    = 1.0   -- scale Size
    h     = -0.5 -- horizontal offset
    v     = 0.9   -- vertical   offset
    offsetM44 =
      mkTransformationMat
      (rot0 * s2)
      (tr0 ^+^ V3 (h + fromIntegral offset*s1) v 0)
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
    '0' -> head drs
    '1' -> drs!!1
    '2' -> drs!!2
    '3' -> drs!!3
    '4' -> drs!!4
    '5' -> drs!!5
    '6' -> drs!!6
    '7' -> drs!!7
    '8' -> drs!!8
    '9' -> drs!!9
    'a' -> drs!!10
    'b' -> drs!!11
    'c' -> drs!!12
    'd' -> drs!!13
    'e' -> drs!!14
    'f' -> drs!!15
    'g' -> drs!!16
    'h' -> drs!!17
    'H' -> drs!!17
    'i' -> drs!!18
    'j' -> drs!!19
    'k' -> drs!!20
    'l' -> drs!!21
    'm' -> drs!!22
    'n' -> drs!!23
    'o' -> drs!!24
    'p' -> drs!!25
    'q' -> drs!!26
    'r' -> drs!!27
    's' -> drs!!28
    't' -> drs!!29
    'u' -> drs!!30
    'v' -> drs!!31
    'w' -> drs!!32
    'W' -> drs!!32
    'x' -> drs!!33
    'y' -> drs!!34
    'z' -> drs!!35
    '+' -> drs!!36
    '-' -> drs!!37
    '=' -> drs!!38
    '>' -> drs!!39
    ',' -> drs!!40
    '.' -> drs!!41
    '?' -> drs!!42
    '!' -> drs!!43
    ' ' -> drs!!44
    _   -> head drs

-- drawableChar :: [Drawable] -> Char -> Drawable
-- drawableChar drs chr =
--   case chr of
--     _   -> head drs

draw :: [Texture] -> [(UUID, GLuint)] ->  BackendOptions -> SDL.Window -> Drawable -> IO ()
draw txs hmap opts window (Drawable name unis (Descriptor vao' numIndices') prog) =
  do
    -- print $ "draw.name : " ++ name
    initUniforms txs unis hmap
    bindVertexArrayObject $= Just vao'

    GL.pointSize $= ptSize opts --0.001
    --GL.pointSmooth $= Enabled

    drawElements (primitiveMode opts) numIndices' GL.UnsignedInt nullPtr

    cullFace  $= Just Back
    depthFunc $= Just Less

-- initResources :: Application -> IO Application
-- initResources app0 =
--   do
--     let
--       objs = introObjs ++ fntObjs ++ fgrObjs ++ bgrObjs
--       txs  = concat $ concatMap (toListOf (materials . traverse . M.textures)) objs :: [Texture]
--       uuids = fmap (view uuid) txs
--       hmap = zip uuids [0..] -- TODO: reserve 0 for font rendering?

--     putStrLn "Initializing Resources..."
--     putStrLn "Loading Textures..."
--     mapM_ (bindTexture hmap) txs
--     putStrLn "Finished loading textures."

--     return app0 { _hmap = hmap }
--       where
--         introObjs = concat $ toListOf (App.objects . O.foreground)  (_intro app0) :: [Object]
--         fntObjs   = concat $ toListOf (App.objects . gui . O.fonts) (_main app0)  :: [Object]
--         fgrObjs   = concat $ toListOf (App.objects . O.foreground)  (_main app0)  :: [Object]
--         bgrObjs   = concat $ toListOf (App.objects . O.background)  (_main app0)  :: [Object]

-- bindTexureUniforms :: [Object] -> IO [(UUID, GLuint)]
-- bindTexureUniforms objs =
--   do
--     print "Loading Textures..."
--     -- tx0s <- mapM (loadTex . view path) txs
--     mapM_ (bindTexture hmap) txs
--     print "Finished loading textures."
--     return hmap
--       where
--         txs   = concat $ concatMap (toListOf (materials . traverse . M.textures)) objs
--         uuids = fmap (view uuid) txs
--         hmap     = zip uuids [0..]
-- -- bindTexture :: [(UUID, GLuint)] -> TextureObject -> IO ()

bindTextureObject :: GLuint -> TextureObject -> IO ()
bindTextureObject txid tx0 = do
  putStrLn $ "Binding Texture Object : " ++ show tx0 ++ " at TextureUnit : " ++ show txid
  texture Texture2D        $= Enabled
  activeTexture            $= TextureUnit txid
  -- tx0 <- loadTex $ view path tx --TODO : replace that with a hashmap lookup?
  textureBinding Texture2D $= Just tx0
    -- where
    --   txid = fromMaybe 0 (lookup (view uuid tx) hmap)

bindTexture :: [(UUID, GLuint)] -> Texture -> IO ()
bindTexture hmap tx =
  do
    -- print $ "bindTexture.tx   : " ++ show tx
    -- print $ "bindTexture.txid : " ++ show txid
    putStrLn $ "Binding Texture : " ++ show tx ++ " at TextureUnit : " ++ show txid
    texture Texture2D        $= Enabled
    activeTexture            $= TextureUnit txid
    tx0 <- loadTex $ view path tx --TODO : replace that with a hashmap lookup?
    textureBinding Texture2D $= Just tx0
      where
        txid = fromMaybe 0 (lookup (view uuid tx) hmap)

-- initUniforms :: [TextureObject] -> Uniforms -> [(UUID, GLuint)] -> IO ()
-- ...
--       mapM_ (allocateTextures program hmap) txs
-- allocateTextures :: Program -> [(UUID, GLuint)] -> Texture -> IO ()      

initUniforms :: [Texture] -> Uniforms -> [(UUID, GLuint)] -> IO ()
initUniforms txs unis hmap =
  do
    let programDebug = loadShaders
                       [ ShaderInfo VertexShader   (FileSource (_vertShader u_mat' ))
                       , ShaderInfo FragmentShader (FileSource (_fragShader u_mat' )) ]
    program <- if debug then programDebug else pure u_prog'
    currentProgram $= Just program

    let u_mouse       = Vector2 (realToFrac $ fst u_mouse') (realToFrac $ snd u_mouse') :: Vector2 GLfloat
    location0         <- get (uniformLocation program "u_mouse'")
    uniform location0 $= u_mouse

    let resX          = fromIntegral $ fromEnum $ fst u_res' :: Double
        resY          = fromIntegral $ fromEnum $ snd u_res' :: Double
        u_res         = Vector2 (realToFrac resX) (realToFrac resY) :: Vector2 GLfloat

    location1         <- get (uniformLocation program "u_resolution")
    uniform location1 $= u_res

    location2         <- get (uniformLocation program "u_time'")
    uniform location2 $= (u_time' :: GLfloat)

    let apt = u_cam_a' -- aperture
        foc = u_cam_f' -- focal length
        proj =
          LP.infinitePerspective
          (2.0 * atan ( apt/2.0 / foc )) -- | FOV
          (resX/resY)                      -- | Aspect
          0.01                           -- | Near

    persp             <- GL.newMatrix RowMajor $ toList' proj   :: IO (GLmatrix GLfloat)
    location3         <- get (uniformLocation program "persp")
    uniform location3 $= persp

    --print $ show u_cam'
    camera            <- GL.newMatrix RowMajor $ toList' u_cam' :: IO (GLmatrix GLfloat)
    location4         <- get (uniformLocation program "camera")
    uniform location4 $= camera

    xform             <- GL.newMatrix RowMajor $ toList' xform' :: IO (GLmatrix GLfloat)
    location5         <- get (uniformLocation program "xform")
    uniform location5 $= xform

    xform1            <- GL.newMatrix RowMajor $ toList' u_xform' :: IO (GLmatrix GLfloat)
    location6         <- get (uniformLocation program "xform1")
    uniform location6 $= xform1

    let sunP = GL.Vector3 299999999999.0 0.0 0.0 :: GL.Vector3 GLfloat
    location7 <- get (uniformLocation program "sunP")
    uniform location7 $= sunP

    -- | Allocate Textures

    -- putStrLn $ "initUniforms.txNames : "  ++ show txNames
    -- putStrLn $ "initUniforms.txuids   : " ++ show txuids
    mapM_ (allocateTextures program hmap) txs

    -- | Unload buffers
    --bindVertexArrayObject         $= Nothing
    --bindBuffer ElementArrayBuffer $= Nothing
      where
        Uniforms u_mat' u_prog' u_mouse' u_time' u_res' u_cam' u_cam_a' u_cam_f' u_xform' = unis
        toList' = fmap realToFrac.concat.(fmap toList.toList) :: V4 (V4 Double) -> [GLfloat]
        xform'  = -- | = Object Position - Camera Position
          transpose $
          fromV3M44
          ( u_xform' ^._xyz )
          ( fromV3V4 (transpose u_xform' ^._w._xyz + transpose u_cam' ^._w._xyz) 1.0 ) :: M44 Double

allocateTextures :: Program -> [(UUID, GLuint)] -> Texture -> IO ()
allocateTextures program hmap tx =
  do
    location <- get (uniformLocation program (view T.name tx))
    uniform location $= TextureUnit txid
      where
        txid = fromMaybe 0 (lookup (view uuid tx) hmap)

fromList :: [a] -> M44 a
fromList xs = V4
              (V4 (head xs ) (xs!!1 )(xs!!2 )(xs!!3))
              (V4 (xs!!4 ) (xs!!5 )(xs!!6 )(xs!!7))
              (V4 (xs!!8 ) (xs!!9 )(xs!!10)(xs!!11))
              (V4 (xs!!12) (xs!!13)(xs!!14)(xs!!15))

fromV3M44 :: V3 (V4 a) -> V4 a -> M44 a
fromV3M44 v3 = V4 (v3 ^. _x) (v3 ^. _y) (v3 ^. _z)

fromV3V4 :: V3 a -> a -> V4 a
fromV3V4 v3 = V4 (v3 ^. _x) (v3 ^. _y) (v3 ^. _z)

nameFromPath :: FilePath -> String
nameFromPath f = head (splitOn "." $ splitOn "/" f!!1)

       -- | Indices -> Stride -> ListOfFloats -> Material -> Descriptor
initVAO :: ([Int], Int, [Float], Material) -> IO Descriptor
initVAO (idx', st', vs', matPath) =
  do
    let
      idx = unsafeCoerce <$> idx' :: [GLuint]
      vs  = unsafeCoerce <$> vs'  :: [GLfloat]
    -- | VAO
    vao <- genObjectName
    bindVertexArrayObject $= Just vao
    -- | VBO
    vertexBuffer <- genObjectName
    bindBuffer ArrayBuffer $= Just vertexBuffer
    withArray vs $ \ptr ->
      do
        let sizev = fromIntegral (length vs * sizeOf (head vs))
        bufferData ArrayBuffer $= (sizev, ptr, StaticDraw)
    -- | EBO
    elementBuffer <- genObjectName
    bindBuffer ElementArrayBuffer $= Just elementBuffer
    let numIndices = length idx
    withArray idx $ \ptr ->
      do
        let indicesSize = fromIntegral (numIndices * sizeOf (head idx))
        bufferData ElementArrayBuffer $= (indicesSize, ptr, StaticDraw)

        -- | Bind the pointer to the vertex attribute data
        let floatSize  = (fromIntegral $ sizeOf (0.0::GLfloat)) :: GLsizei
            stride     = fromIntegral st' * floatSize

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

loadTex :: FilePath -> IO TextureObject
loadTex f =
  do
    t <- either error id <$> readTexture f
    texture2DWrap $= (Repeated, ClampToEdge)
    textureFilter  Texture2D $= ((Linear', Just Nearest), Linear')
    blend $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    generateMipmap' Texture2D
    return t

genTex :: Sz2 -> Ptr Word8 -> IO TextureObject
genTex s ptr = do
  -- TODO: convert array (of pixel data) into a TexInfo object (feed it to OpenGL Texture Object)
  -- https://hackage.haskell.org/package/GLUtil-0.10.4/docs/src/Graphics.GLUtil.JuicyTextures.html#readTexture
  -- https://hackage.haskell.org/package/GLUtil-0.10.4/docs/Graphics-GLUtil-Textures.html#t:TexInfo
  -- https://hackage.haskell.org/package/OpenGL-3.0.3.0/docs/Graphics-Rendering-OpenGL-GL-Texturing-Objects.html#t:TextureObject
  -- let txInfo = texInfo w h TexMono ptr :: TexInfo (Ptr Word8) -- -> TextureObject
  -- let txInfo = texInfo w h TexRGBA ptr :: TexInfo (Ptr Word8)
  -- let txInfo = texInfo w h TexRGB ptr :: TexInfo (Ptr Word8)
  let txInfo = texInfo 128 128 TexRGBA ptr :: TexInfo (Ptr Word8)
      (A.Sz2 w h) = s
  --let txInfo = texInfo 100 100 TexMono ptr :: TexInfo (Ptr Word8) -- -> TextureObject
  -- https://hackage.haskell.org/package/GLUtil-0.10.4/docs/Graphics-GLUtil-Textures.html#v:loadTexture
  t <- loadTexture txInfo -- :: IO TextureObject
  texture2DWrap $= (Repeated, ClampToEdge)
  textureFilter  Texture2D $= ((Linear', Just Nearest), Linear')
  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  generateMipmap' Texture2D
  return t
      --where (A.Sz2 w h) = s
