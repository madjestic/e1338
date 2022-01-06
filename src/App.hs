{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}

module App
  ( App     (..)
  , Options (..)
  , options
  , App.name
  , App.resx
  , App.resy
  , App.objects
  , playCam
  , App.cameras
  , initApp
  ) where

import Control.Lens
import Foreign.C                 (CInt)
import Unsafe.Coerce

import Camera
import Descriptor
import Object
import Project     as P

-- import Debug.Trace as DT

data App
  = App
  {
    _debug     :: (Double, Double)
  , _options   :: Options
  , _objects   :: ObjectTree
  , _playCam   :: Camera
  , _cameras   :: [Camera]
  } deriving Show

data Options
  = Options
  { _name  :: String
  , _resx  :: CInt
  , _resy  :: CInt
  } deriving Show

$(makeLenses ''Options)
$(makeLenses ''App)

-- -- < Init App State > ------------------------------------------------------

initApp ::
     (([Int], Int, [Float]) -> IO Descriptor)
  -> Project
  -> IO App
initApp initVAO project =
  do
    putStrLn   "initializing app resources..."
    putStrLn $ "project name : " ++ view P.name project ++ "\n"
    objTree <- initObjectTree initVAO project

    let
      cams = fromProjectCamera <$> view P.cameras project
      pCam = head cams
      -- camerasP = Utils.fromList <$> toListOf (P.cameras . traverse . pTransform) project
      -- playCamP = head camerasP --fromList $ camerasP!!0
    --pc <- fromVGeo $ fromPGeo pCloud  -- PCloud Point Cloud
    --let objTree = [pc]
    let app =
          App
          (-42,-17)
          ( Options
            name'
            resX'
            resY'
          )
          --Main
          objTree
          pCam
          cams

    print "finished initializing app resources..."
    return app
      where
        name' = view P.name project
        resX' = (unsafeCoerce $ view P.resx project) :: CInt
        resY' = (unsafeCoerce $ view P.resy project) :: CInt
