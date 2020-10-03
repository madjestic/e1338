{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Arrows #-}

module Controllable
  ( Controllable (..)
  , Device (..)
  , Keyboard (..)
  , Mouse (..)
  , transform
  , transform'
  , ypr      
  , device
  , device'
  , mouse
  , keyboard
  , updateController
  ) where

import Linear.Matrix
import Linear.V3
import Linear.Quaternion

import Control.Lens hiding (transform)
import FRP.Yampa hiding (identity)--(SF, Event, returnA, isEvent, lMerge, catEvents, )
import SDL.Input.Keyboard.Codes as SDL
import Data.Functor              (($>))

import Keyboard
import Mouse
import AppInput
import Utils ()

import Debug.Trace as DT

data Controllable
  =  Controller
     {
       _debug      :: (Double, Double)
     , _transform  :: M44 Double
     , _ypr        :: V3 Double  -- yaw/pitch/roll
     , _device     :: Device
     }
  |  Solver
     {
--       _pivot      :: V3 Double
       _transform  :: M44 Double
     , _ypr        :: V3 Double  -- yaw/pitch/roll
--     , _velocity   :: V3 Double
--     , _physC      :: Physics -- TODO : add phys.parms
     }
  deriving Show

data Device
  =  Device
     {
       _keyboard :: Keyboard 
     , _mouse    :: Mouse    
     } deriving Show

transform' :: Lens' Controllable (M44 Double)
transform' = lens _transform (\controllable newTransform -> Solver { _transform = newTransform })

device'    :: Lens' Controllable Device
device'    = lens _device    (\controllable newDevice    -> Controller { _device    = newDevice })

$(makeLenses ''Device)
$(makeLenses ''Controllable)

-- | ~inspired by foldrWith mtx0 keys - for every keyInput apply a folding transform to mtx0
updateController :: Controllable -> SF AppInput Controllable
updateController ctl0 =
  switch sf cont
  where
    sf = 
      proc input -> do
        (mouse', mevs) <- updateMouse ctl0    -< input
        (mrx,    mry)  <- mouseRelPos         -< input
        (kbrd',  kevs) <- updateKeyboard ctl0 -< input

        let
          keyVecs1 = keyVecs kbrd'
          ypr1  =
            (1500 * (V3 mry mrx 0.0) +) $
            (50000 *) $ 
            foldr1 (+) $
            zipWith (*^) ((\x -> if x then (1.0::Double) else 0) . ($ keys kbrd') <$>
                          [ keyUp,  keyDown, keyLeft, keyRight, keyQ,  keyE ])
                          [ pPitch, nPitch,  pYaw,    nYaw,     pRoll, nRoll ]
            where
              pPitch = (keyVecs1)!!6  -- positive  pitch
              nPitch = (keyVecs1)!!7  -- negative  pitch
              pYaw   = (keyVecs1)!!8  -- positive  yaw
              nYaw   = (keyVecs1)!!9  -- negative  yaw
              pRoll  = (keyVecs1)!!10 -- positive  roll
              nRoll  = (keyVecs1)!!11 -- negative  roll
        
        ypr'     <- ((V3 0 0 0) ^+^) ^<< integral -< ypr1

        let
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
                          [keyW, keyS, keyA, keyD, keyZ, keyC])
                          [fVel, bVel, lVel, rVel, uVel, dVel]
       
            where fVel   = (keyVecs1)!!0  -- forwards  velocity
                  bVel   = (keyVecs1)!!1  -- backwards velocity
                  lVel   = (keyVecs1)!!2  -- left      velocity
                  rVel   = (keyVecs1)!!3  -- right     velocity
                  uVel   = (keyVecs1)!!4  -- up        velocity
                  dVel   = (keyVecs1)!!5  -- down      velocity
                  
                  baseSpeed     = 5000000
                  shift  = keyLShift $ (keys kbrd')
                  ctl    = keyLCtrl  $ (keys kbrd')
                  scalar = s shift ctl
                  s shift ctl
                    | shift && ctl = baseSpeed*baseSpeed*0.5 -- superfast
                    | shift     = baseSpeed * 10000  -- fast
                    | ctl       = baseSpeed * 0.1   -- slow
                    | otherwise = baseSpeed         -- base speed
    
        tr'  <- ((view translation (Controllable._transform ctl0)) ^+^) ^<< integral -< tr1
               
        let
          mtx' = 
            mkTransformationMat
            rot
            tr'
            
        let result =
              ctl0
              { Controllable._transform = mtx' -- DT.trace ("mtx' :" ++ show mtx') $ mtx'
              , Controllable._ypr       = ypr'
              , Controllable._device =
                  (_device ctl0) {_mouse = mouse', _keyboard = kbrd' }
              }
     
        returnA -<
          ( result
          , catEvents (mevs ++ kevs) $> result ) 
          where
            mtx0 = view Controllable.transform ctl0
    cont c = updateController c

updateKeyboard :: Controllable -> SF AppInput (Keyboard, [Event ()])
updateKeyboard ctl0 =
  proc input -> do
        (kkeys, kevs) <- updateKeys  ctl0 -< input
        let
          events = [(catEvents kevs) $> ()]
          keyboard = (_keyboard._device $ ctl0) { keys = kkeys}
          
        returnA -< (keyboard, events)

updateKeys :: Controllable -> SF AppInput (Keys, [Event ()])
updateKeys ctl0 = 
  proc input -> do
    (keyW_, keyWe) <- keyEvent SDL.ScancodeW keyW ctl0 -< input
    (keyS_, keySe) <- keyEvent SDL.ScancodeS keyS ctl0 -< input
    (keyA_, keyAe) <- keyEvent SDL.ScancodeA keyA ctl0 -< input
    (keyD_, keyDe) <- keyEvent SDL.ScancodeD keyD ctl0 -< input

    (keyQ_, keyQe) <- keyEvent SDL.ScancodeQ keyQ ctl0 -< input
    (keyE_, keyEe) <- keyEvent SDL.ScancodeE keyE ctl0 -< input
    (keyZ_, keyZe) <- keyEvent SDL.ScancodeZ keyZ ctl0 -< input
    (keyC_, keyCe) <- keyEvent SDL.ScancodeC keyC ctl0 -< input
    (keyPageUp_,   keyPageUpE)   <- keyEvent SDL.ScancodePageUp   keyPageUp   ctl0 -< input
    (keyPageDown_, keyPageDownE) <- keyEvent SDL.ScancodePageDown keyPageDown ctl0 -< input

    (keyLShift_, keyLShiftE) <- keyEvent SDL.ScancodeLShift keyLShift ctl0 -< input
    (keyLCtrl_ , keyLCtrlE)  <- keyEvent SDL.ScancodeLCtrl  keyLCtrl  ctl0 -< input
  
    (keyUp_,    keyUpE)    <- keyEvent SDL.ScancodeUp    keyUp    ctl0 -< input
    (keyDown_,  keyDownE)  <- keyEvent SDL.ScancodeDown  keyDown  ctl0 -< input
    (keyLeft_,  keyLeftE)  <- keyEvent SDL.ScancodeLeft  keyLeft  ctl0 -< input
    (keyRight_, keyRightE) <- keyEvent SDL.ScancodeRight keyRight ctl0 -< input

    let events = [      keyWe, keySe, keyAe, keyDe, keyQe, keyEe, keyZe, keyCe, keyUpE, keyDownE, keyLeftE,   keyRightE,    keyPageUpE, keyPageDownE, keyLShiftE, keyLCtrlE ]
        keys   = ( Keys keyW_  keyS_  keyA_  keyD_  keyQ_  keyE_  keyZ_  keyC_  keyUp_  keyDown_  keyLeft_    keyRight_     keyPageUp_  keyPageDown_  keyLShift_  keyLCtrl_ )

    returnA -< (keys, events)

keyEvent :: SDL.Scancode -> (Keys -> Bool) -> Controllable -> SF AppInput (Bool, Event ())
keyEvent code keyFunc ctl = 
  proc input -> do
    keyPressed     <- keyInput code  "Pressed"  -< input
    keyReleased    <- keyInput code  "Released" -< input
    let
      keys0  = keys._keyboard._device $ ctl
      result = keyState (keyFunc keys0) keyPressed keyReleased
      event  = lMerge keyPressed keyReleased
    returnA -< (result, event)

keyState :: Bool -> Event () -> Event () -> Bool
keyState state pressed released
  | isEvent pressed  = True
  | isEvent released = False
  | otherwise        = state

updateMouse :: Controllable -> SF AppInput (Mouse, [Event ()])
updateMouse ctl0 = 
  proc input -> do
-- TODO: Should be something along the lines with:
-- _ <- mouseEvents -< input
    mouseMovedE   <- mouseMovedEvent   ctl0 -< input -- relative motion event
    mouseStoppedE <- mouseStoppedEvent ctl0 -< input
    rpos <- mouseRelPos -< input
--    pos <- 
    let
      --event  = mouseMovedE $> ():: Event ()
      events = [(mouseMovedE $> ())
               ,(mouseStoppedE $> ())]
      mouse  =
        Mouse
        (lmb._mouse._device $ ctl0)
        (rmb._mouse._device $ ctl0)
        (_pos._mouse._device $ ctl0)
        ((\e -> if isEvent e then fromEvent e else (0,0)) mouseMovedE)
        ((\e -> if isEvent e then False else True) mouseStoppedE)
        []
    returnA -< (mouse, events)

mouseMovedEvent :: Controllable -> SF AppInput (Event (Double, Double))
mouseMovedEvent ctl =
  proc input -> do
    mouseMoved <- mouseMoving  -< input
    returnA -< mouseMoved
    
mouseStoppedEvent :: Controllable -> SF AppInput (Event ())
mouseStoppedEvent ctl =
  proc input -> do
    mouseStop <- mouseStopped -< input
    returnA -< mouseStop

-- is mouse moving or stopped?
mouseState :: Bool -> Event () -> Event () -> Bool
mouseState state moving stopped
  | isEvent moving  = True
  | isEvent stopped = False
  | otherwise       = state    
