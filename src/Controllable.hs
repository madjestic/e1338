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
  ) where

import Linear.Matrix
import Linear.V3
import Linear.Quaternion

import Control.Lens hiding (transform)
-- import FRP.Yampa    hiding (identity)
import SDL.Input.Keyboard.Codes as SDL
import Data.Functor              (($>))

import Keyboard
import Mouse
-- import AppInput
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
