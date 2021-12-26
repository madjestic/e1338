{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Arrows #-}

module Camera
  ( Camera (..)
  , defaultCam
  , controller
  , mouseS
  , keyboardRS
  , keyboardTS
  , defaultCamController
  , updateCameras
  , updateCamera
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
       _name       :: String
     , _apt        :: Double
     , _foc        :: Double 
     , _controller :: Controllable
     , _mouseS     :: V3 Double -- | mouse    "sensitivity"
     , _keyboardRS :: V3 Double -- | keyboard "rotation sensitivity"
     , _keyboardTS :: V3 Double -- | keyboard "translation sensitivity"
-- TODO: move sensitivity parms here     
     } deriving Show

$(makeLenses ''Camera)

defaultCam :: Camera
defaultCam =
  Camera
  "PlayerCamera" -- | Player Camera
  50.0
  100.0
  defaultCamController
  1.0
  1.0
  1.0

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
          (kbrd',  kevs) <- updateKeyboard (view (controller.device.keyboard) cam0) -< (input, (view (controller.device.keyboard) cam))

          let
            s'       = 1.0  :: Double -- | mouse sensiticity scale
            t'       = 2    :: Double -- | mouse idle threshold
            rlag     = 0.95           -- | rotation    stop lag/innertia
            tlag     = 0.9            -- | translation stop lag/innertia
            
            ypr'     =
              (view (controller.ypr) cam +) $
              (0.00001 * (view mouseS cam) *
                (V3 (case (abs mry' <= t') of True -> 0; _ -> (mry' / t') * (abs mry')**s')
                    (case (abs mrx' <= t') of True -> 0; _ -> (mrx' / t') * (abs mrx')**s')
                     0.0) +) $
              foldr1 (+) $
              fmap ( 0.0000001 * scalar * (view keyboardRS cam) *) $ -- <- make it keyboard controllabe: speed up/down            
              zipWith (*^) ((\x -> if x then (1.0::Double) else 0) . ($ keys kbrd') <$>
                            [ keyUp,  keyDown, keyLeft, keyRight, keyPageUp,  keyPageDown ])
                            [ pPitch, nPitch,  pYaw,    nYaw,     pRoll, nRoll ]
              where
                (mrx', mry') = view rpos mouse'
                pPitch = (keyVecs kbrd')!!6  -- positive  pitch
                nPitch = (keyVecs kbrd')!!7  -- negative  pitch
                pYaw   = (keyVecs kbrd')!!8  -- positive  yaw
                nYaw   = (keyVecs kbrd')!!9  -- negative  yaw
                pRoll  = (keyVecs kbrd')!!10 -- positive  roll
                nRoll  = (keyVecs kbrd')!!11 -- negative  roll
           
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

          let
            ctl0 = view controller cam -- change cam to cam0 has a drastic difference
            mtx0 = view Controllable.transform ctl0
            rot =
              (view _m33 mtx0)
              !*! fromQuaternion (axisAngle (view _x (view _m33 mtx0)) (view _x ypr')) -- yaw
              !*! fromQuaternion (axisAngle (view _y (view _m33 mtx0)) (view _y ypr')) -- pitch
              !*! fromQuaternion (axisAngle (view _z (view _m33 mtx0)) (view _z ypr')) -- roll
   
            vel' =
              (view (controller.vel) cam +) $
              foldr1 (+) $
              fmap ( 0.1 * (scalar) * (view keyboardTS cam) *) $ -- <- make it keyboard controllabe: speed up/down
              fmap (transpose (rot) !*) $
              zipWith (*^) ((\x -> if x then (1::Double) else 0) . ($ (keys kbrd')) <$>
                            [keyW, keyS, keyA, keyD, keyQ, keyE])
                            [fVel, bVel, lVel, rVel, uVel, dVel]
   
              where fVel   = (keyVecs kbrd')!!0  -- forwards  velocity
                    bVel   = (keyVecs kbrd')!!1  -- backwards velocity
                    lVel   = (keyVecs kbrd')!!2  -- left      velocity
                    rVel   = (keyVecs kbrd')!!3  -- right     velocity
                    uVel   = (keyVecs kbrd')!!4  -- up        velocity
                    dVel   = (keyVecs kbrd')!!5  -- down      velocity
   
                    baseSpeed     = 5000000
                    ctl    = keyLCtrl  $ (keys kbrd')
                    shift  = keyLShift $ (keys kbrd')
                    alt    = keyLAlt   $ (keys kbrd')
                    scalar = s ctl shift alt
                    s ctl shift alt
                      | ctl && shift && alt = baseSpeed^2 * 100 -- superduperfast
                      | ctl && shift = baseSpeed^2 * 0.5        -- superfast
                      | shift        = baseSpeed   * 100        -- fast
                      | ctl          = baseSpeed   * 0.1          -- slow
                      | otherwise    = baseSpeed                -- base speed

          let
            tr'  = (view translation (Controllable._transform (view controller cam)) +) vel'
            mtx' =
              mkTransformationMat
              rot
              tr'
   
          let
            dev' = view (controller.device) cam
            ctl' = (view controller cam)
                   {
                     Controllable._transform = mtx'
                   , Controllable._vel       = vel' * tlag
                   , Controllable._ypr       = ypr' * rlag
                   , Controllable._device    =
                       (dev' { _keyboard = kbrd'
                             , _mouse    = mouse' })
                   }
            result =
              cam {Camera._controller = ctl'}
                       
          returnA -<
            ( result
            , catEvents (kevs ++ ((tagWith ()) <$> mevs)) $> result )

    cont = updateCameraController

-- updateCameraController' :: SF (AppInput, Camera) Camera
-- updateCameraController' =
--   -- switch sf cont
--   -- where
--   --   sf =
--       proc (input, cam) ->
--         do
--           (mouse', mevs) <- updateMouse         -< input
--           (kbrd',  kevs) <- updateKeyboard' (view (controller.device.keyboard)) -< (input, (view (controller.device.keyboard) cam))

--           let
--             s'       = 1.0  :: Double -- | mouse sensiticity scale
--             t'       = 2    :: Double -- | mouse idle threshold
--             rlag     = 0.95           -- | rotation    stop lag/innertia
--             tlag     = 0.9            -- | translation stop lag/innertia
            
--             ypr'     =
--               (view (controller.ypr) cam +) $
--               (0.00001 *
--                 (V3 (case (abs mry' <= t') of True -> 0; _ -> (mry' / t') * (abs mry')**s')
--                     (case (abs mrx' <= t') of True -> 0; _ -> (mrx' / t') * (abs mrx')**s')
--                      0.0) +) $
--               foldr1 (+) $
--               fmap ( 0.0000001 * scalar *) $ -- <- make it keyboard controllabe: speed up/down            
--               zipWith (*^) ((\x -> if x then (1.0::Double) else 0) . ($ keys kbrd') <$>
--                             [ keyUp,  keyDown, keyLeft, keyRight, keyPageUp,  keyPageDown ])
--                             [ pPitch, nPitch,  pYaw,    nYaw,     pRoll, nRoll ]
--               where
--                 (mrx', mry') = view rpos mouse'
--                 pPitch = (keyVecs kbrd')!!6  -- positive  pitch
--                 nPitch = (keyVecs kbrd')!!7  -- negative  pitch
--                 pYaw   = (keyVecs kbrd')!!8  -- positive  yaw
--                 nYaw   = (keyVecs kbrd')!!9  -- negative  yaw
--                 pRoll  = (keyVecs kbrd')!!10 -- positive  roll
--                 nRoll  = (keyVecs kbrd')!!11 -- negative  roll
           
--                 baseSpeed     = 5000
--                 ctl    = keyLCtrl  $ (keys kbrd')
--                 shift  = keyLShift $ (keys kbrd')
--                 alt    = keyLAlt   $ (keys kbrd')
--                 scalar = s ctl shift alt
--                 s ctl shift alt
--                   | ctl && shift = baseSpeed * 2     -- superfast
--                   | ctl && alt   = baseSpeed * 0.01  -- very slow
--                   | shift        = baseSpeed * 1.5   -- fast
--                   | alt          = baseSpeed * 0.1   -- slow
--                   | otherwise    = baseSpeed         -- base speed

--           let
--             ctl0 = view controller cam -- change cam to cam0 has a drastic difference
--             mtx0 = view Controllable.transform ctl0
--             rot =
--               (view _m33 mtx0)
--               !*! fromQuaternion (axisAngle (view _x (view _m33 mtx0)) (view _x ypr')) -- yaw
--               !*! fromQuaternion (axisAngle (view _y (view _m33 mtx0)) (view _y ypr')) -- pitch
--               !*! fromQuaternion (axisAngle (view _z (view _m33 mtx0)) (view _z ypr')) -- roll
   
--             vel' =
--               (view (controller.vel) cam +) $
--               foldr1 (+) $
--               fmap ( 0.1 * (scalar) *) $ -- <- make it keyboard controllabe: speed up/down
--               fmap (transpose (rot) !*) $
--               zipWith (*^) ((\x -> if x then (1::Double) else 0) . ($ (keys kbrd')) <$>
--                             [keyW, keyS, keyA, keyD, keyQ, keyE])
--                             [fVel, bVel, lVel, rVel, uVel, dVel]
   
--               where fVel   = (keyVecs kbrd')!!0  -- forwards  velocity
--                     bVel   = (keyVecs kbrd')!!1  -- backwards velocity
--                     lVel   = (keyVecs kbrd')!!2  -- left      velocity
--                     rVel   = (keyVecs kbrd')!!3  -- right     velocity
--                     uVel   = (keyVecs kbrd')!!4  -- up        velocity
--                     dVel   = (keyVecs kbrd')!!5  -- down      velocity
   
--                     baseSpeed     = 5000000
--                     ctl    = keyLCtrl  $ (keys kbrd')
--                     shift  = keyLShift $ (keys kbrd')
--                     alt    = keyLAlt   $ (keys kbrd')
--                     scalar = s ctl shift alt
--                     s ctl shift alt
--                       | ctl && shift && alt = baseSpeed^2 * 100 -- superduperfast
--                       | ctl && shift = baseSpeed^2 * 0.5        -- superfast
--                       | shift        = baseSpeed   * 100        -- fast
--                       | ctl          = baseSpeed   * 0.1          -- slow
--                       | otherwise    = baseSpeed                -- base speed

--           let
--             tr'  = (view translation (Controllable._transform (view controller cam)) +) vel'
--             mtx' =
--               mkTransformationMat
--               rot
--               tr'
   
--           let
--             dev' = view (controller.device) cam
--             ctl' = (view controller cam)
--                    {
--                      Controllable._transform = mtx'
--                    , Controllable._vel       = vel' * tlag
--                    , Controllable._ypr       = ypr' * rlag
--                    , Controllable._device    =
--                        (dev' { _keyboard = kbrd'
--                              , _mouse    = mouse' })
--                    }
--             result =
--               cam {Camera._controller = ctl'}
                       
--           returnA -<
--             ( result
--             , catEvents (kevs ++ ((tagWith ()) <$> mevs)) $> result )

-- --    cont = updateCameraController'
    
updateCamera' :: SF (AppInput, Camera) Camera
updateCamera' = undefined
  -- proc (input, cam) -> do
  --   cam <- updateCameraController' -< (input, cam)
  --   returnA -< cam

updateCamera :: Camera -> SF (AppInput, Camera) Camera
updateCamera cam0 = 
  proc (input, cam) -> do
    rec cam  <- iPre cam0 -< cam'
        cam' <- updateCameraController cam0 -< (input, cam)
    -- cam <- updateCameraController cam0 -< (input, cam)
    returnA -< cam

updateCameras :: ([Camera], Camera) -> SF (AppInput, Camera) ([Camera], Camera)
updateCameras (cams0, cam0) =
  switch sf cont
  where
    sf =
      proc (input, cam') -> do
        cams <- switchCameras cams0 -< ()
        cam  <- updateCamera  $ head cams0 -< (input, cam')

        kev <- keyInput SDL.ScancodeC "Pressed" -< input -- switch camera

        let
          result  = (cams0, cam)  
          result' = (cams,  cam)
          
        returnA -<
          ( result
          , kev $> result' )
          
    cont = updateCameras

updateCameras' :: SF (AppInput, ([Camera], Camera)) ([Camera], Camera)
updateCameras' = undefined
--  switch sf cont
  -- where
  --   sf =
  --     proc (input, (cams0, cam0)) -> do
  --       cams <- switchCameras' -< cams0
  --       cam  <- updateCamera'  -< (input, cam0)

  --       kev <- keyInput SDL.ScancodeC "Pressed" -< input -- switch camera

  --       let
  --         result  = (cams0, cam)  
  --         result' = (cams,  cam)
          
  --       returnA -<
  --         ( result
  --         , kev $> result' )
          
  --   cont = updateCameras'

switchCameras' :: SF [Camera] [Camera]
switchCameras' =
  proc cams ->
    do
      let result = rotateList 1 cams
      returnA -< result

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
    _apt        = _pApt pcam
  , _foc        = _pFoc pcam
  , _controller =
      defaultCamController
      { _transform = fromList ( _pTransform pcam) }
  , _mouseS     = pure $ _pMouseS pcam    :: V3 Double
  , _keyboardRS = pure $ _pKeyboardRS pcam :: V3 Double
  , _keyboardTS = pure $ _pKeyboardTS pcam :: V3 Double
  }
