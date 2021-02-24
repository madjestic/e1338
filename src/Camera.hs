{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Arrows #-}

module Camera
  ( Camera (..)
  , defaultCam
  , controller
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
          (kbrd',  kevs) <- updateKeyboard (view (controller.device.keyboard) cam0) -< (input, (view (controller.device.keyboard) cam))

          let
            s'       = 1.0 :: Double -- | mouse sensiticity scale
            t'       = 2   :: Double -- | mouse idle threshold
            keyVecs1 = keyVecs kbrd'
            --keyVecs1 = keyVecs (DT.trace ("kbrd' keys :" ++ show (keys kbrd')) kbrd')
            ypr1     =
              --(view (controller.ypr) cam +) $ -- and this
              --(10 * (V3 mry' mrx' 0.0) +) $
              (10 * (V3 (mry' * (abs mry')**s') (mrx' * (abs mrx')**s') 0.0) +) $
              -- (10 *
              --   (V3 (case (abs mry' <= t') of True -> 0; _ -> (mry' / t') * (abs mry')**s')
              --       (case (abs mrx' <= t') of True -> 0; _ -> (mrx' / t') * (abs mrx')**s')
              --        0.0) +) $
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

          ypr'     <- ((V3 0 0 0) ^+^) ^<< integral -< ypr1 -- This
          --ypr'     <- ((view (controller.ypr) cam0) ^+^) ^<< integral -< ypr1

          let
            ctl0 = view controller cam0 -- change cam to cam0 has a drastic difference
            mtx0 = view Controllable.transform ctl0
            rot = -- identity :: M33 Double
              (view _m33 mtx0)
              !*! fromQuaternion (axisAngle (view _x (view _m33 mtx0)) (view _x ypr')) -- yaw
              !*! fromQuaternion (axisAngle (view _y (view _m33 mtx0)) (view _y ypr')) -- pitch
              !*! fromQuaternion (axisAngle (view _z (view _m33 mtx0)) (view _z ypr')) -- roll
   
            tr1  = -- V3
              (view (controller.vel) cam +) $
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
          vel' <- ((V3 0 0 0) ^+^) ^<< integral -< tr1
   
          let
            mtx' =
              mkTransformationMat
              rot
              tr'
              -- (DT.trace ("tr' :" ++ show tr') tr')
   
          let
            dev'   = view (controller.device) cam
            ctl'   = (view controller cam)
                     {
                       Controllable._transform = mtx'
                     , Controllable._vel       = vel'
                     , Controllable._ypr       = ypr'
                     , Controllable._device    =
                         (dev' { _keyboard = kbrd'
                               , _mouse    = mouse' })
                     }
            result = cam {Camera._controller = ctl'}
                       
          returnA -<
            ( result
            , catEvents (kevs ++ ((tagWith ()) <$> mevs)) $> result )

    cont = updateCameraController
    --cont = updateCameraLinear

updateCameraController' :: Camera -> SF (AppInput, Camera) Camera
updateCameraController' cam0 =
  switch sf cont
  where
    sf =
      proc (input, cam) ->
        do
          (mouse', mevs) <- updateMouse         -< input
          (kbrd',  kevs) <- updateKeyboard (view (controller.device.keyboard) cam0) -< (input, (view (controller.device.keyboard) cam))

          let
            s'       = 1.0 :: Double -- | mouse sensiticity scale
            t'       = 2   :: Double -- | mouse idle threshold
            keyVecs1 = keyVecs kbrd'
            --keyVecs1 = keyVecs (DT.trace ("kbrd' keys :" ++ show (keys kbrd')) kbrd')
            ypr1     =
              (view (controller.ypr) cam +) $ -- and this
              --(10 * (V3 mry' mrx' 0.0) +) $
              --(10 * (V3 (mry' * (abs mry')**s') (mrx' * (abs mrx')**s') 0.0) +) $
              (0.00001 *
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

          --ypr'     <- ((V3 0 0 0) ^+^) ^<< integral -< ypr1 -- This
          --ypr'     <- ((view (controller.ypr) cam0) ^+^) ^<< integral -< ypr1

          let
            ypr' = ypr1 -- This
            ctl0 = view controller cam -- change cam to cam0 has a drastic difference
            mtx0 = view Controllable.transform ctl0
            rot = -- identity :: M33 Double
              (view _m33 mtx0)
              !*! fromQuaternion (axisAngle (view _x (view _m33 mtx0)) (view _x ypr')) -- yaw
              !*! fromQuaternion (axisAngle (view _y (view _m33 mtx0)) (view _y ypr')) -- pitch
              !*! fromQuaternion (axisAngle (view _z (view _m33 mtx0)) (view _z ypr')) -- roll
   
            tr1  = -- V3
              (view (controller.vel) cam +) $
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
                      | shift        = baseSpeed * 10000        -- fast
                      | ctl          = baseSpeed * 0.1          -- slow
                      | otherwise    = baseSpeed                -- base speed

          tr'  <- ((view translation (Controllable._transform (view controller cam0))) ^+^) ^<< integral -< tr1
          vel' <- ((V3 0 0 0) ^+^) ^<< integral -< tr1
   
          let
            mtx' =
              mkTransformationMat
              rot
              tr'
              -- (DT.trace ("tr' :" ++ show tr') tr')
   
          let
            dev'   = view (controller.device) cam
            ctl'   = (view controller cam)
                     {
                       Controllable._transform = mtx'
                     , Controllable._vel       = vel'
                     , Controllable._ypr       = ypr'*0.95
                     , Controllable._device    =
                         (dev' { _keyboard = kbrd'
                               , _mouse    = mouse' })
                     }
            result =
              DT.trace ("ypr' :" ++ show ypr') $
              DT.trace ("ypr1 :" ++ show ypr1) $
              DT.trace ("ypr0 :" ++ show (view (controller.ypr) cam0)) $
              DT.trace ("mevs :" ++ show mevs) $
              cam {Camera._controller = ctl'}
                       
          returnA -<
            ( result
            , catEvents (kevs ++ ((tagWith ()) <$> mevs)) $> result )

    cont = updateCameraController'1
    --cont = updateCameraLinear

updateCameraController'1 :: Camera -> SF (AppInput, Camera) Camera
updateCameraController'1 cam0 =
  switch sf cont
  where
    sf =
      proc (input, cam) ->
        do
          (mouse', mevs) <- updateMouse         -< input
          (kbrd',  kevs) <- updateKeyboard (view (controller.device.keyboard) cam0) -< (input, (view (controller.device.keyboard) cam))

          let
            s'       = 1.0 :: Double -- | mouse sensiticity scale
            t'       = 2   :: Double -- | mouse idle threshold
            keyVecs1 = keyVecs kbrd'
            --keyVecs1 = keyVecs (DT.trace ("kbrd' keys :" ++ show (keys kbrd')) kbrd')
            ypr1     =
              (view (controller.ypr) cam +) $ -- and this
              --(10 * (V3 mry' mrx' 0.0) +) $
              --(10 * (V3 (mry' * (abs mry')**s') (mrx' * (abs mrx')**s') 0.0) +) $
              (0.00001 *
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

          --ypr'     <- ((V3 0 0 0) ^+^) ^<< integral -< ypr1 -- This
          --ypr'     <- ((view (controller.ypr) cam0) ^+^) ^<< integral -< ypr1

          let
            ypr' = ypr1
            ctl0 = view controller cam -- change cam to cam0 has a drastic difference
            mtx0 = view Controllable.transform ctl0
            rot = -- identity :: M33 Double
              (view _m33 mtx0)
              !*! fromQuaternion (axisAngle (view _x (view _m33 mtx0)) (view _x ypr')) -- yaw
              !*! fromQuaternion (axisAngle (view _y (view _m33 mtx0)) (view _y ypr')) -- pitch
              !*! fromQuaternion (axisAngle (view _z (view _m33 mtx0)) (view _z ypr')) -- roll
   
            tr1  = -- V3
              (view (controller.vel) cam +) $
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
                      | shift        = baseSpeed * 10000        -- fast
                      | ctl          = baseSpeed * 0.1          -- slow
                      | otherwise    = baseSpeed                -- base speed

          tr'  <- ((view translation (Controllable._transform (view controller cam0))) ^+^) ^<< integral -< tr1
          vel' <- ((V3 0 0 0) ^+^) ^<< integral -< tr1
   
          let
            mtx' =
              mkTransformationMat
              rot
              tr'
              -- (DT.trace ("tr' :" ++ show tr') tr')
   
          let
            dev'   = view (controller.device) cam
            ctl'   = (view controller cam)
                     {
                       Controllable._transform = mtx'
                     , Controllable._vel       = vel'
                     , Controllable._ypr       = ypr'*0.95
                     , Controllable._device    =
                         (dev' { _keyboard = kbrd'
                               , _mouse    = mouse' })
                     }
            result =
              DT.trace ("ypr' :" ++ show ypr') $
              DT.trace ("ypr1 :" ++ show ypr1) $
              DT.trace ("ypr0 :" ++ show (view (controller.ypr) cam0)) $
              DT.trace ("mevs :" ++ show mevs) $
              cam {Camera._controller = ctl'}
                       
          returnA -<
            ( result
            , catEvents (kevs ++ ((tagWith ()) <$> mevs)) $> result )

    cont = updateCameraController'

updateCameraController'' :: Camera -> SF (AppInput, Camera) Camera
updateCameraController'' cam0 =
      proc (input, cam) ->
        do
          (mouse', mevs) <- updateMouse         -< input
          (kbrd',  kevs) <- updateKeyboard (view (controller.device.keyboard) cam0) -< (input, (view (controller.device.keyboard) cam))

          let
            s'       = 1.0 :: Double -- | mouse sensiticity scale
            t'       = 2   :: Double -- | mouse idle threshold
            keyVecs1 = keyVecs kbrd'
            --keyVecs1 = keyVecs (DT.trace ("kbrd' keys :" ++ show (keys kbrd')) kbrd')
            ypr1     =
              --(view (controller.ypr) cam +) $ -- and this
              --(10 * (V3 mry' mrx' 0.0) +) $
              --(10 * (V3 (mry' * (abs mry')**s') (mrx' * (abs mrx')**s') 0.0) +) $
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

          ypr'     <- ((V3 0 0 0) ^+^) ^<< integral -< ypr1 -- This
          --let ypr' = V3 0 0 0
          -- ypr'' <- 0.5 * ^<< integral -< ypr'
          -- let ypr' = ypr'' * 0.25
          -- let ypr0 = view (controller.ypr) cam
          --ypr'     <- ((view (controller.ypr) cam0) ^+^) ^<< integral -< ypr1

          let
            ctl0 = view controller cam -- change cam to cam0 has a drastic difference
            mtx0 = view Controllable.transform ctl0
            rot = -- identity :: M33 Double
              (view _m33 mtx0)
              !*! fromQuaternion (axisAngle (view _x (view _m33 mtx0)) (view _x ypr')) -- yaw
              !*! fromQuaternion (axisAngle (view _y (view _m33 mtx0)) (view _y ypr')) -- pitch
              !*! fromQuaternion (axisAngle (view _z (view _m33 mtx0)) (view _z ypr')) -- roll
   
            tr1  = -- V3
              (view (controller.vel) cam +) $
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
          vel' <- ((V3 0 0 0) ^+^) ^<< integral -< tr1
   
          let
            mtx' =
              mkTransformationMat
              rot
              tr'
              -- (DT.trace ("tr' :" ++ show tr') tr')
   
          let
            dev'   = view (controller.device) cam
            ctl'   = (view controller cam)
                     {
                       Controllable._transform = mtx'
                     , Controllable._vel       = vel'
                     --, Controllable._ypr       = ypr'*0.5
                     , Controllable._device    =
                         (dev' { _keyboard = kbrd'
                               , _mouse    = mouse' })
                     }
            result =
              DT.trace ("ypr' :" ++ show ypr') $
              DT.trace ("ypr1 :" ++ show ypr1) $
              DT.trace ("ypr0 :" ++ show (view (controller.ypr) cam0)) $
              DT.trace ("mevs :" ++ show mevs) $
              cam {Camera._controller = ctl'}
                       
          returnA -< result

updateCameraLinear :: Camera -> SF (AppInput, Camera) Camera
updateCameraLinear cam0 = 
  switch sf cont
  where
    sf =
      proc (input, cam) -> do
        
        (mouse', mevs) <- updateMouse         -< input
        (kbrd',  kevs) <- updateKeyboard (view (controller.device.keyboard) cam0) -< (input, (view (controller.device.keyboard) cam))

        -- let
        --   tr1 = (view (controller.vel) cam) :: V3 Double
        tr'  <- ((view translation (Controllable._transform (view controller cam0))) ^+^) ^<< integral -< V3 1000000000 0 0--tr1

        let
          ctl0 = view controller cam
          mtx0 = view Controllable.transform ctl0
          rot = -- identity :: M33 Double
            (view _m33 mtx0)          
          mtx' =
            mkTransformationMat
            rot
            tr'
            -- (DT.trace ("tr' :" ++ show tr') tr')

        let
          ypr'   = view (controller.ypr) cam
          dev'   = view (controller.device) cam
          ctl'   = (view controller cam)
                   {
                     Controllable._transform = mtx'
                   --, Controllable._vel       = vel'
                   , Controllable._ypr       = ypr'*0.5
                   , Controllable._device    =
                       (dev' { _keyboard = kbrd'
                             , _mouse    = mouse' })
                   }
          --result = cam {Camera._controller = ctl'}
          result = DT.trace ("Linear") $ cam0 {Camera._controller = ctl'}
                       
        returnA -<
            ( result
            , catEvents (kevs ++ ((tagWith ()) <$> mevs)) $> result )
            
    cont = updateCameraController''

updateCamera :: Camera -> SF (AppInput, Camera) Camera
updateCamera cam0 = 
  proc (input, cam) -> do
    rec cam  <- iPre cam0 -< cam'
        --cam' <- updateCameraLinear cam0 -< (input, cam)
        cam' <- updateCameraController' cam0 -< (input, cam)
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
