{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}

module Application
  ( Application (..)
  , Main        (..)
  , Interface   (..)
  , fromApplication
--  , hmap
  , init
  ) where

import Control.Lens
import Data.UUID
import Graphics.Rendering.OpenGL as GL    (GLuint)

import App

-- import Debug.Trace as DT

data Main = Default
  deriving Show

data Interface =
     Intro
   | Main Main
   | Finished
   | Menu
  deriving Show

data Application
  = Application
  {
    _interface :: Interface
  , _intro     :: App
  , _main      :: App
  , _hmap      :: [(UUID, GLuint)] -- a placeholder for the future hmap, for now it's a map from a long texture unit index to a short version.
                                   -- UUID -> GLuint
  } deriving Show
$(makeLenses ''Application)

fromApplication :: Application -> App
fromApplication app =
  case (view interface app) of
  --case (view interface (DT.trace ("fromApplication.app :" ++ show app) app)) of
    Intro        -> view intro app
    Main Default -> view main  app
    _ -> view main app
