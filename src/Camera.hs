{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Arrows #-}

module Camera
  ( Camera (..)
  , defaultCam
  , controller
  , defaultCamController
  , updateCamera
  , updateCamera'
  , updateCameras
  , updateCameras'
  , fromProjectCamera
  ) where

import Control.Lens
import Linear                    (V3(..), V4 (..))
import Linear.Matrix
import Linear.V3
import Linear.Quaternion
import FRP.Yampa
import Data.Functor              (($>))
import SDL.Input.Keyboard.Codes as SDL

import Controllable
import Keyboard
import Mouse
import AppInput
import Utils
import Project

import Debug.Trace as DT

data Camera =
     Camera
     {
       _apt        :: Double
     , _foc        :: Double 
     , _controller :: Controllable
     } deriving Show

$(makeLenses ''Camera)

defaultCam :: Camera
defaultCam =
  Camera
  50.0
  100.0
  defaultCamController

defaultCamController :: Controllable
defaultCamController =
  ( Controller
    (0,0)
    -- (transpose (identity :: M44 Double))
    (
      (V4
        (V4 1 0 0 0)
        (V4 0 1 0 0) -- <- . . . y ...
        (V4 0 0 1 0) -- <- . . . z-component of transform
        (V4 0 0 0 1)))
    (V3 0 0 0) -- velocity
    (V3 0 0 0) -- rotation
    (Device (Keyboard keys0 kvs0) (Mouse Nothing Nothing (0,0) (0.0, 0.0) False mvs0 )))
  where
    mvs0   = [] --undefined
    -- mvs0 - mouse vectors
    keys0  = ( Keys False False False False False False False False False False False False False False False False False )
    -- kvs0 - key vectors keyVecs
    kvs0   = [ fVel, bVel, lVel, rVel, uVel, dVel, pPitch, nPitch, pYaw, nYaw, pRoll, nRoll ]
    fVel   = V3 ( 0  )( 0  )( 0.1)   -- forwards  velocity
    bVel   = V3 ( 0  )( 0  )(-0.1)   -- backwards velocity
    lVel   = V3 ( 0.1)( 0  )( 0  )   -- left      velocity
    rVel   = V3 (-0.1)( 0  )( 0  )   -- right     velocity
    uVel   = V3 ( 0  )(-0.1)( 0  )   -- right     velocity
    dVel   = V3 ( 0  )( 0.1)( 0  )   -- right     velocity
    pPitch = V3 (-1.0)( 0  )( 0  )   -- positive  pitch
    nPitch = V3 ( 1.0)( 0  )( 0  )   -- negative  pitch
    pYaw   = V3 ( 0  )(-1.0)( 0  )   -- positive  yaw
    nYaw   = V3 ( 0  )( 1.0)( 0  )   -- negative  yaw
    pRoll  = V3 ( 0  )(  0 )(-1.0)   -- positive  roll
    nRoll  = V3 ( 0  )(  0 )( 1.0)   -- negative  roll

updateCameraController :: Camera -> SF (AppInput, Camera) Camera
updateCameraController cam0 =
  switch sf cont
  where
    sf =
      proc (input, cam) ->
        do
          (mouse', mevs) <- updateMouse         -< input
          --(kbrd',  kevs) <- updateKeyboard (view controller cam0) -< input
          (kbrd',  kevs) <- updateKeyboard' (view (controller.device.keyboard) cam0) -< (input, (view (controller.device.keyboard) cam))

          let
            s'       = 1.5 :: Double -- | mouse sensiticity scale
            t'       = 1   :: Double -- | mouse idle threshold
            keyVecs1 = keyVecs kbrd'
            --keyVecs1 = keyVecs (DT.trace ("kbrd' keys :" ++ show (keys kbrd')) kbrd')
            ypr1     =
              (view (controller.ypr) cam +) $
              --(10 * (V3 mry' mrx' 0.0) +) $
              (10 *
                (V3 (case (abs mry' <= t') of True -> 0; _ -> (mry' / t') * (abs mry')**s')
                    (case (abs mrx' <= t') of True -> 0; _ -> (mrx' / t') * (abs mrx')**s')
                     0.0) +) $
              foldr1 (+) $
              fmap (scalar *) $ -- <- make it keyboard controllabe: speed up/down            
              zipWith (*^) ((\x -> if x then (1.0::Double) else 0) . ($ keys kbrd') <$>
                            [ keyUp,  keyDown, keyLeft, keyRight, keyPageUp,  keyPageDown ])
                            [ pPitch, nPitch,  pYaw,    nYaw,     pRoll, nRoll ]
              where
                (mrx', mry') = view rpos mouse'
                pPitch = (keyVecs1)!!6  -- positive  pitch
                nPitch = (keyVecs1)!!7  -- negative  pitch
                pYaw   = (keyVecs1)!!8  -- positive  yaw
                nYaw   = (keyVecs1)!!9  -- negative  yaw
                pRoll  = (keyVecs1)!!10 -- positive  roll
                nRoll  = (keyVecs1)!!11 -- negative  roll
           
                baseSpeed     = 5000
                ctl    = keyLCtrl  $ (keys kbrd')
                shift  = keyLShift $ (keys kbrd')
                alt    = keyLAlt   $ (keys kbrd')
                scalar = s ctl shift alt
                s ctl shift alt
                  | ctl && shift = baseSpeed * 2     -- superfast
                  | ctl && alt   = baseSpeed * 0.01  -- very slow
                  | shift        = baseSpeed * 1.5   -- fast
                  | alt          = baseSpeed * 0.1   -- slow
                  | otherwise    = baseSpeed         -- base speed

          ypr'     <- ((V3 0 0 0) ^+^) ^<< integral -< ypr1
          -- ypr'     <- ((V3 0 0 0) ^+^) ^<< integral -< (DT.trace ("ypr :"  ++ show (view (controller.ypr) cam) ++ "\n" ++
          --                                                         "ypr1 :" ++ show ypr1) ypr1)
          -- ypr'     <- ((view (controller.ypr) cam0) ^+^) ^<< integral -< (DT.trace ("ypr :"  ++ show (view (controller.ypr) cam) ++ "\n" ++
          --                                                                          "ypr1 :" ++ show ypr1) ypr1)
          --ypr'     <- (ypr0 ^+^) ^<< integral -< ypr1
   
          let
            ctl0 = view controller cam0
            mtx0 = view Controllable.transform ctl0
            rot = -- identity :: M33 Double
              (view _m33 mtx0)
              !*! fromQuaternion (axisAngle (view _x (view _m33 mtx0)) (view _x ypr')) -- yaw
              !*! fromQuaternion (axisAngle (view _y (view _m33 mtx0)) (view _y ypr')) -- pitch
              !*! fromQuaternion (axisAngle (view _z (view _m33 mtx0)) (view _z ypr')) -- roll
   
            tr1  = -- V3
              foldr1 (+) $
              fmap ((scalar) *) $ -- <- make it keyboard controllabe: speed up/down
              fmap (transpose (rot) !*) $
              zipWith (*^) ((\x -> if x then (1::Double) else 0) . ($ (keys kbrd')) <$>
                            [keyW, keyS, keyA, keyD, keyQ, keyE])
                            [fVel, bVel, lVel, rVel, uVel, dVel]
   
              where fVel   = (keyVecs1)!!0  -- forwards  velocity
                    bVel   = (keyVecs1)!!1  -- backwards velocity
                    lVel   = (keyVecs1)!!2  -- left      velocity
                    rVel   = (keyVecs1)!!3  -- right     velocity
                    uVel   = (keyVecs1)!!4  -- up        velocity
                    dVel   = (keyVecs1)!!5  -- down      velocity
   
                    baseSpeed     = 5000000
                    ctl    = keyLCtrl  $ (keys kbrd')
                    shift  = keyLShift $ (keys kbrd')
                    alt    = keyLAlt   $ (keys kbrd')
                    scalar = s ctl shift alt
                    s ctl shift alt
                      | ctl && shift && alt = baseSpeed^2 * 100 -- superduperfast
                      | ctl && shift = baseSpeed^2 * 0.5        -- superfast
                      | shift        = baseSpeed * 10000  -- fast
                      | ctl          = baseSpeed * 0.1    -- slow
                      | otherwise    = baseSpeed          -- base speed

          tr'  <- ((view translation (Controllable._transform (view controller cam0))) ^+^) ^<< integral -< tr1
   
          let
            mtx' =
              mkTransformationMat
              rot
              tr'
              -- (DT.trace ("tr' :" ++ show tr') tr')
   
          let
            dev'   = view (controller.device) cam
            --kbd'   = view keyboard dev'
            ctl'   = (view controller cam)
                     {
                       Controllable._transform = mtx'
                     , Controllable._ypr       = ypr'
                     , Controllable._device    = (dev' {_keyboard = kbrd'})
                     }
            result = cam {Camera._controller = ctl'}
                       
                -- ctl'
                -- { Controllable._transform = mtx' -- DT.trace ("mtx' :" ++ show mtx') $ mtx'
                -- --, Controllable._ypr       = ypr'
                -- , Controllable._device =
                --     (_device ctl0) { _keyboard = kbrd'
                --                    , _mouse    = mouse' }
                -- }
                
          returnA -<
            ( result
            , catEvents (kevs ++ ((tagWith ()) <$> mevs)) $> result )

    cont = updateCameraController

updateCamera' :: Camera -> SF (AppInput, Camera) Camera
updateCamera' cam0 = 
  proc (input, cam) -> do
    rec cam  <- iPre cam0 -< cam'
        cam' <- updateCameraController cam0 -< (input, cam)
    -- cam <- updateCameraController cam0 -< (input, cam)
    returnA -< cam

updateCamera :: Camera -> SF AppInput Camera
updateCamera cam0 = 
  proc input ->
    do
      ctl <- updateController (view controller cam0) -< (input, (view controller cam0))
      let
        cam = cam0 { Camera._controller = ctl }
      returnA -< cam

updateCameras :: ([Camera], Camera) -> SF AppInput ([Camera], Camera)
updateCameras (cams0, cam0) =
  switch sf cont
  where
    sf =
      proc input -> do
        cams <- switchCameras cams0 -< ()
        cam  <- updateCamera  $ head cams0 -< input

        kev <- keyInput SDL.ScancodeC "Pressed" -< input -- switch camera

        let
          result  = (cams0, cam)  
          result' = (cams,  cam)
          
        returnA -<
          ( result
          , kev $> result' )
          
    cont = updateCameras

updateCameras' :: ([Camera], Camera) -> SF (AppInput, Camera) ([Camera], Camera)
updateCameras' (cams0, cam0) =
  switch sf cont
  where
    sf =
      proc (input, cam') -> do
        cams <- switchCameras cams0 -< ()
        cam  <- updateCamera'  $ head cams0 -< (input, cam')

        kev <- keyInput SDL.ScancodeC "Pressed" -< input -- switch camera

        let
          result  = (cams0, cam)  
          result' = (cams,  cam)
          
        returnA -<
          ( result
          , kev $> result' )
          
    cont = updateCameras'

switchCameras :: [Camera] -> SF () [Camera]
switchCameras cams0 =
  proc input ->
    do
      let result = rotateList 1 cams0
      returnA -< result

fromProjectCamera :: ProjectCamera -> Camera
fromProjectCamera pcam =
  defaultCam
  {
    _apt = ( _pApt pcam)
  , _foc = ( _pFoc pcam)
  , _controller =
      defaultCamController
      { _transform = fromList ( _pTransform pcam) }
  }
