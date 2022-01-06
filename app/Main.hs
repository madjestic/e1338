{-# LANGUAGE CPP    #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings, Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Main where 

import Control.Concurrent ( swapMVar, newMVar, readMVar, MVar )
import Control.Lens       ( toListOf, view )
import Data.Set           ( fromList, toList )
import Data.Text          ( pack)
import Foreign.C          ( CInt )
import FRP.Yampa as FRP   ( (>>>), reactimate, Arrow((&&&)), Event(..), SF )
import SDL
    ( pollEvent,
      setMouseLocationMode,
      time,
      glSwapWindow,
      --ticks,
      Event(eventPayload),
      EventPayload,
      LocationMode(AbsoluteLocation, RelativeLocation),
      Window )
import Graphics.Rendering.OpenGL ( PrimitiveMode(..), Color4 (Color4), clear, clearColor, ($=), Program, ClearBuffer (..))
import System.Environment        ( getArgs )
import Unsafe.Coerce             ( unsafeCoerce )
import Linear.Matrix
    
import Application
import Update (handleExit, appRun)
import Project as P ( camMode, resy, resx, name, read )
import AppInput                  ( parseWinInput ) 
import Rendering as R
import Material as M
import qualified Texture  as T
import Drawable
import Camera
import Controllable
import Object             as O
import Descriptor
import Texture
import Mouse
import Utils ((<$.>), (<*.>))
import App

-- import Debug.Trace    as DT

debug :: Bool
#ifdef DEBUG
debug = True
#else
debug = False
#endif

-- -- < Animate > ------------------------------------------------------------
type WinInput = FRP.Event SDL.EventPayload
type WinOutput = (Application, Bool)

animate :: SDL.Window
        -> SF WinInput WinOutput  -- ^ signal function to animate
        -> IO ()
animate window sf =
  do
    reactimate (return NoEvent)
               senseInput
               renderOutput
               sf
    closeWindow window
    
      where
        senseInput _ =
          do
            lastInteraction <- newMVar =<< SDL.time
            currentTime <- SDL.time                          
            dt <- (currentTime -) <$> swapMVar lastInteraction currentTime --dtime
            mEvent <- SDL.pollEvent
            
            return (dt, Event . SDL.eventPayload <$> mEvent)
            
        renderOutput _ (app, shouldExit) =
          do
            lastInteraction <- newMVar =<< SDL.time

            output lastInteraction window app
            return shouldExit

toDrawable :: App -> [Object] -> Double -> [Drawable]
toDrawable app objs time0 = drs -- (drs, drs')
  where
    mpos = unsafeCoerce $ view (playCam . controller . device' . mouse . pos) app -- :: (Double, Double)
    resX = fromEnum $ view (options . App.resx) app :: Int
    resY = fromEnum $ view (options . App.resy) app :: Int
    res  = (toEnum resX, toEnum resY) :: (CInt, CInt)
    cam  = view playCam app :: Camera
    drs  = concatMap (toDrawable' mpos time0 res cam) objs :: [Drawable]

toDrawable' :: (Double, Double) -> Double -> (CInt, CInt) -> Camera -> Object -> [Drawable]
toDrawable' mpos time0 res cam obj = drs
  where
    drs      =
      (\u_mats' u_prog' u_mouse' u_time' u_res' u_cam' u_cam_a' u_cam_f' u_xform' ds' ps' name'
        -> Drawable name' (Uniforms u_mats' u_prog' u_mouse' u_time' u_res' u_cam' u_cam_a' u_cam_f' u_xform') ds' ps')
      <$.> mats <*.> progs <*.> mpos_ <*.> time_ <*.> res_ <*.> cam_ <*.> cam_a_ <*.> cam_f_ <*.> xforms <*.> ds <*.> progs <*.> names

    n      = length $ view descriptors obj:: Int
    mpos_  = replicate n mpos  :: [(Double, Double)]
    time_  = replicate n time0 :: [Double]
    res_   = replicate n res   :: [(CInt, CInt)]
    cam_   = replicate n $ view (controller . Controllable.transform) cam  :: [M44 Double]
    cam_a_ = replicate n $ _apt cam :: [Double]
    cam_f_ = replicate n $ _foc cam :: [Double]

    names  = toListOf (O.materials . traverse . M.name) obj :: [String]
    mats   = view O.materials   obj :: [Material]
    progs  = view O.programs    obj :: [Program]
    xforms = concat $ replicate n $ view O.transforms obj :: [M44 Double]
    ds     = view O.descriptors obj :: [Descriptor]

output :: MVar Double -> Window -> Application -> IO ()
output lastInteraction window application = do

  -- ticks   <- SDL.ticks
  -- let currentTime = fromInteger (unsafeCoerce ticks :: Integer) :: Float

-- | render FPS current
  currentTime <- SDL.time
  -- dt <- (currentTime -) <$> readMVar lastInteraction

  let
    fntObjs = concat $ toListOf (objects . gui . fonts) app :: [Object]
    fgrObjs = concat $ toListOf (objects . foreground)  app :: [Object]
    bgrObjs = concat $ toListOf (objects . background)  app :: [Object]

    fntsDrs = toDrawable app fntObjs currentTime :: [Drawable]
    objsDrs = toDrawable app fgrObjs currentTime :: [Drawable]
    bgrsDrs = toDrawable app bgrObjs currentTime :: [Drawable]

    app  = fromApplication application
    txs  = concat $ toListOf ( traverse . materials . traverse . textures) (fgrObjs ++ fntObjs) :: [Texture]
    hmap = _hmap application

    opts =
      BackendOptions
      { primitiveMode = Triangles
      , bgrColor      = Color4 0.0 0.0 0.0 1.0
      , ptSize        = 1.0 }
    
  clearColor $= bgrColor opts --Color4 0.0 0.0 0.0 1.0
  clear [ColorBuffer, DepthBuffer]

  mapM_ (render txs hmap (opts { primitiveMode = Triangles })) objsDrs
  mapM_ (render txs hmap (opts { primitiveMode = Points })) bgrsDrs

  currentTime' <- SDL.time
  dt <- (currentTime' -) <$> readMVar lastInteraction
  renderString (render txs hmap (opts { primitiveMode = Triangles })) fntsDrs $ "fps:" ++ show (round (1/dt) :: Integer)
  
  -- TODO:
  -- render' front
  -- render' backs
  -- render' fonts
    
  glSwapWindow window


-- < Main Function > -----------------------------------------------------------

initResources :: Application -> IO Application
initResources app0 =
  do
    let
      objs = introObjs ++ fntObjs ++ fgrObjs ++ bgrObjs
      txs  = concat $ concatMap (toListOf (materials . traverse . M.textures)) objs -- :: [Texture]
      uuids = fmap (view T.uuid) txs

      hmap = toList . fromList $ zip uuids [0..]

    putStrLn "Initializing Resources..."
    putStrLn "Loading Textures..."
    mapM_ (bindTexture hmap) txs
    putStrLn "Finished loading textures."

    return app0 { _hmap = hmap }
      where
        introObjs = concat $ toListOf (App.objects . O.foreground)  (_intro app0) :: [Object]
        fntObjs   = concat $ toListOf (App.objects . gui . O.fonts) (_main app0)  :: [Object]
        fgrObjs   = concat $ toListOf (App.objects . O.foreground)  (_main app0)  :: [Object]
        bgrObjs   = concat $ toListOf (App.objects . O.background)  (_main app0)  :: [Object]


main :: IO ()
main = do

  --let argsDebug = return ["./projects/intro", "./projects/view_model"]
  let argsDebug = return ["./projects/intro_XXII", "./projects/solar_system"]
  args <- if debug then argsDebug else getArgs

  introProj <- P.read (unsafeCoerce (args!!0) :: FilePath)
  mainProj  <- P.read (unsafeCoerce (args!!1) :: FilePath)
  
  let
    title   = pack $ view P.name mainProj
    resX    = (unsafeCoerce $ view P.resx mainProj) :: CInt
    resY    = (unsafeCoerce $ view P.resy mainProj) :: CInt

  window    <- openWindow
               title
               (resX, resY)

  -- | SDL Mouse Options
  let camMode' =
        case view P.camMode mainProj of
          "RelativeLocation" -> RelativeLocation
          "AbsoluteLocation" -> AbsoluteLocation
          _ -> error "wrong mouse mode"

  _ <- setMouseLocationMode camMode'

  putStrLn "\n Initializing App"
  intro <- initApp initVAO introProj
  main' <- initApp initVAO mainProj

  let
    initApp' =
      Application
      Intro
      intro
      main'
      []

  app <- initResources initApp'
  
  putStrLn "Starting App."
  animate
    window
    (parseWinInput >>> appRun app &&& handleExit)
  return ()    
