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
