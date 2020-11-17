{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Arrows #-}

module Camera
  ( Camera (..)
  , defaultCam
  , controller
  , defaultCamController
  , updateCamera
  , updateCameras
  , fromProjectCamera
  ) where

import Control.Lens
import Linear                    (V3(..), V4 (..))
import FRP.Yampa -- (SF, returnA)
import Data.Functor              (($>))
import SDL.Input.Keyboard.Codes as SDL

import Controllable
import Keyboard
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

updateCamera :: Camera -> SF AppInput Camera
updateCamera cam0 = 
  proc input ->
    do
      ctl' <- updateController (view controller cam0) -< input -- add a switch for mouse moving / stopped, switch between integral and static
      let
        cam' = cam0 { Camera._controller = ctl' }

      returnA -< cam'

updateCameras :: ([Camera], Camera) -> SF AppInput ([Camera], Camera)
updateCameras (cams0, cam0) =
  switch sf cont
  where
    sf =
      proc input -> do
        cams <- switchCameras cams0 -< ()
        cam  <- updateCamera  $ cams0!!0 -< input

        kev <- keyInput SDL.ScancodeC "Pressed" -< input

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
