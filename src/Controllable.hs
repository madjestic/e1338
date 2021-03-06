{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}

module Controllable
  ( Controllable (..)
  , Device (..)
  , Keyboard (..)
  , Mouse (..)
  , transform
--  , transform'
  , ypr
  , vel
  , device
  , device'
  , mouse
  , keyboard
  , updateKeyboard
  , updateKeyboard'
  , updateMouse
  ) where

import Linear.Matrix
import Linear.V3
import Linear.Quaternion

import Control.Lens hiding (transform)
import FRP.Yampa    hiding (identity)
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
     , _vel        :: V3 Double  -- velocity
     , _ypr        :: V3 Double  -- yaw/pitch/roll
     , _device     :: Device     -- store as index in the proj file: 0 - keyboard, 1 - mouse, etc.
     }
--   |  Solver
--      {
-- --       _pivot      :: V3 Double
--        _transform  :: M44 Double
--      , _ypr        :: V3 Double  -- yaw/pitch/roll
-- --     , _velocity   :: V3 Double
-- --     , _physC      :: Physics -- TODO : add phys.parms
--      }
  deriving Show

data Device
  =  Device
     {
       _keyboard :: Keyboard
     , _mouse    :: Mouse
     } deriving Show

-- transform' :: Lens' Controllable (M44 Double)
-- transform' = lens _transform (\controllable newTransform -> Solver { _transform = newTransform })

device'    :: Lens' Controllable Device
device'    = lens _device    (\controllable newDevice    -> Controller { _device    = newDevice })

$(makeLenses ''Device)
$(makeLenses ''Controllable)

updateKeyboard' :: SF (AppInput, Keyboard) (Keyboard, [Event ()])
updateKeyboard' = undefined

-- | ~inspired by foldrWith mtx0 keys - for every keyInput apply a folding transform to mtx0
updateKeyboard :: Keyboard -> SF (AppInput, Keyboard) (Keyboard, [Event ()])
updateKeyboard kbd0 =
  proc (input, kbd) -> do
        (keys', kevs) <- updateKeys (keys kbd0) -< (input, (keys kbd))
        let
          events = [(catEvents kevs) $> ()]
          kbd' = kbd { keys = keys' }

        returnA -< (kbd', events)

updateKeys :: Keys -> SF (AppInput, Keys) (Keys, [Event ()])
updateKeys keys0 =
  proc (input, keys) -> do
    (keyW_, keyWe) <- keyEvent SDL.ScancodeW keyW keys0 -< input
    (keyS_, keySe) <- keyEvent SDL.ScancodeS keyS keys0 -< input
    (keyA_, keyAe) <- keyEvent SDL.ScancodeA keyA keys0 -< input
    (keyD_, keyDe) <- keyEvent SDL.ScancodeD keyD keys0 -< input

    (keyQ_, keyQe) <- keyEvent SDL.ScancodeQ keyQ keys0 -< input
    (keyE_, keyEe) <- keyEvent SDL.ScancodeE keyE keys0 -< input
    (keyZ_, keyZe) <- keyEvent SDL.ScancodeZ keyZ keys0 -< input
    (keyC_, keyCe) <- keyEvent SDL.ScancodeC keyC keys0 -< input
    (keyPageUp_,   keyPageUpE)   <- keyEvent SDL.ScancodePageUp   keyPageUp   keys0 -< input
    (keyPageDown_, keyPageDownE) <- keyEvent SDL.ScancodePageDown keyPageDown keys0 -< input

    (keyLShift_, keyLShiftE) <- keyEvent SDL.ScancodeLShift keyLShift keys0 -< input
    (keyLCtrl_ , keyLCtrlE)  <- keyEvent SDL.ScancodeLCtrl  keyLCtrl  keys0 -< input
    (keyLAlt_ , keyLAltE)    <- keyEvent SDL.ScancodeLAlt   keyLAlt   keys0 -< input

    (keyUp_,    keyUpE)    <- keyEvent SDL.ScancodeUp    keyUp    keys0 -< input
    (keyDown_,  keyDownE)  <- keyEvent SDL.ScancodeDown  keyDown  keys0 -< input
    (keyLeft_,  keyLeftE)  <- keyEvent SDL.ScancodeLeft  keyLeft  keys0 -< input
    (keyRight_, keyRightE) <- keyEvent SDL.ScancodeRight keyRight keys0 -< input

    let events = [      keyWe, keySe, keyAe, keyDe, keyQe, keyEe, keyZe, keyCe, keyUpE, keyDownE, keyLeftE,   keyRightE,    keyPageUpE, keyPageDownE, keyLShiftE, keyLCtrlE, keyLAltE ]
        keys'  = ( Keys keyW_  keyS_  keyA_  keyD_  keyQ_  keyE_  keyZ_  keyC_  keyUp_  keyDown_  keyLeft_    keyRight_     keyPageUp_  keyPageDown_  keyLShift_  keyLCtrl_  keyLAlt_ )

    returnA -< (keys', events)

keyEvent :: SDL.Scancode -> (Keys -> Bool) -> Keys -> SF AppInput (Bool, Event ())
keyEvent code keyFunc keys0 =
  proc input -> do
    keyPressed     <- keyInput code  "Pressed"  -< input
    keyReleased    <- keyInput code  "Released" -< input
    let
      result = keyState (keyFunc keys0) keyPressed keyReleased
      event  = lMerge keyPressed keyReleased
    returnA -< (result, event)

keyState :: Bool -> Event () -> Event () -> Bool
keyState state pressed released
  | isEvent pressed  = True
  | isEvent released = False
  | otherwise        = state

updateMouse :: SF AppInput (Mouse, [Event (Double, Double)])
updateMouse =
  proc input -> do
    lmbE <- lbpPos       -< input
    rmbE <- rbpPos       -< input
    mmovE <- mouseMoving -< input

    mpos' <- mousePos     -< input
    rpos' <- mouseRelPos  -< input

    let
      events = [lmbE, rmbE, mmovE]
      mouse  =
        Mouse
        (case isEvent lmbE of
           True -> Just $ fromEvent lmbE
           _    -> Nothing)
        (case isEvent rmbE of
           True -> Just $ fromEvent rmbE
           _    -> Nothing)
        mpos'
        rpos'
        (isEvent mmovE)
        []
    returnA -< (mouse, events)
    --returnA -< (mouse, DT.trace ("mouse :" ++ show events) events)

-- is mouse moving or stopped?
mouseState :: Bool -> Event () -> Event () -> Bool
mouseState state moving stopped
  | isEvent moving  = True
  | isEvent stopped = False
  | otherwise       = state
