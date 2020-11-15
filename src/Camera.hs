{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Arrows #-}

module Camera
  ( Camera (..)
  , defaultCam
  , controller
  , defaultCamController
  , updateCamera
--  , updateCamera'  
  , updateCameras
  , switchCameras
  , switchCameras'
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
  200.0
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
        (V4 0 0 1 (-60)) -- <- . . . z-component of transform
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

-- controller :: Lens' Camera Controllable
-- controller = lens _controller (\camera newController -> Camera { _controller = newController })

-- TODO: add camera switcher, based on a key-event, somewhere here.
updateCamera :: Camera -> SF AppInput Camera
updateCamera cam0 = 
  proc input ->
    do
      ctl' <- updateController (view controller cam0) -< input -- add a switch for mouse moving / stopped, switch between integral and static
      let
        cam' = cam0 { Camera._controller = ctl' }
      --returnA -< (DT.trace ("cam' :" ++ show cam') $ cam')
      returnA -< cam'

-- updateCamera' :: [Camera] -> Int -> SF (AppInput, Game) Camera
-- updateCamera' cams idx = 
--   proc input ->
--     do
--       ctl' <- updateController (view controller (cams!!idx)) -< input -- add a switch for mouse moving / stopped, switch between integral and static
--       let
--         cam0 = cams!!idx
--         cam' = cam0 { Camera._controller = ctl' }
--       --returnA -< (DT.trace ("cam' :" ++ show cam') $ cam')
--       returnA -< cam'

updateCameras :: ([Camera], Camera) -> SF AppInput ([Camera], Camera)
updateCameras (cams0, cam0) = 
  proc input ->
    do
      cams1 <- switchCameras cams0 -< ()
      ctl' <- updateController (view controller cam0) -< input
      let
        cam0' = cam0 { Camera._controller = ctl' }
      --returnA -< (DT.trace ("cam' :" ++ show cam') $ cam')
      returnA -< (cams1, cam0)

-- TODO: revolve a list as a result of an event
switchCameras :: [Camera] -> SF () [Camera]
switchCameras cams0 =
  proc input ->
    do
      let result = rotateList 1 cams0
      returnA -< result

switchCameras' :: [Camera] -> SF AppInput [Camera]
switchCameras' cams0 =
  switch sf cont
  where
    sf =
      proc input ->
        do
          kev <- keyInput SDL.ScancodeC "Pressed" -< input
          let cams1 = rotateList 1 cams0 -- (DT.trace ("switchCameras' cams0 :" ++ show cams0 ++ "\n") $ cams0)

          returnA -<
            ( cams0
            , kev $> cams1 )
            -- ( (DT.trace ("switchCameras' cams0 :" ++ show cams0 ++ "\n") $ cams0)
            -- , kev' $> (DT.trace ("switchCameras' cams1 :" ++ show cams1 ++ "\n") $ cams1) )

    cont = switchCameras'

-- updateCamera' :: Camera -> SF AppInput Camera
-- updateCamera' cam0 = 
--   switch sf cont
--   where
--     sf = --undefined
--       proc input ->
--         do
--           ctl' <- updateController (view controller cam0) -< input -- add a switch for mouse moving / stopped, switch between integral and static
--       --     (kbrd',  kevs) <- updateKeyboard (view controller cam0) -< input
--           let
--             cam' = cam0 { Camera._controller = ctl' }
--       --     --returnA -< (DT.trace ("cam' :" ++ show cam') $ cam')
--           returnA -< ( cam'
--                      , kevs $> cam' )
--     cont = upda
