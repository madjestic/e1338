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
  , updateApp
  , handleExit
  , centerView
  , initApp
  ) where

import Control.Lens
import Data.Functor              (($>))
import FRP.Yampa
import Foreign.C                 (CInt)
import Linear.Matrix
import SDL.Input.Keyboard.Codes as SDL
import Unsafe.Coerce
import Data.List.Index          as DL     (indexed)
import Data.IntMap.Lazy         as IM

import AppInput
import Camera
import Controllable
import Descriptor
import Material
import Object
import Project     as Prj
import Utils
import Solvable

import Debug.Trace as DT

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

updateApp :: App -> SF AppInput App
updateApp app =
  proc input -> do
    (cams, cam) <- updateCameras (App._cameras app, App._playCam app) -< (input, App._playCam app)

    objs        <- updateObjects        filteredLinObjs -< ()
    let objsIntMap = IM.fromList (zip filteredLinObjsIdxs objs)
    
    objs'       <- updateObjects' filteredNonLinObjs -< filteredNonLinObjs
    let objs'IntMap = IM.fromList (zip filteredNonLinObjsIdxs objs')

    let
      unionObjs = IM.union objs'IntMap objsIntMap
      objTree = App._objects app
      result =
        app { App._objects = (objTree {_foreground = snd <$> IM.toList unionObjs})
            , App._cameras = cams
            , _playCam      = cam
            }

    returnA  -< result
      where
        idxObjs    = DL.indexed $ _foreground (App._objects app)
        intObjMap  = IM.fromList idxObjs :: IntMap Object
        
        filterNonLinIntObjMap  = IM.filter (any (\case Gravity {} -> True; _ -> False) . view Object.solvers) intObjMap
        filteredNonLinObjs     = snd <$> IM.toList filterNonLinIntObjMap
        filteredNonLinObjsIdxs = fst <$> IM.toList filterNonLinIntObjMap

        filterLinIntObjMap  = IM.filter (any (\case Gravity {} -> False; _ -> True) . view Object.solvers) intObjMap
        filteredLinObjs     = snd <$> IM.toList filterLinIntObjMap
        filteredLinObjsIdxs = fst <$> IM.toList filterLinIntObjMap

handleExit :: SF AppInput Bool
handleExit = quitEvent >>^ isEvent

centerView :: SF AppInput Bool
centerView = centerEvent >>^ isEvent


-- -- < Init App State > ------------------------------------------------------

initApp ::
     (([Int], Int, [Float], Material) -> IO Descriptor)
  -> Project
  -> IO App
initApp initVAO project =
  do
    print   "initializing app resources..."
    print $ "project name :" ++ view Prj.name project
    objTree <- initObjectTree initVAO project

    let
      cams = fromProjectCamera <$> view Prj.cameras project
      pCam = head cams
      camerasP = Utils.fromList <$> toListOf (Prj.cameras . traverse . pTransform) project
      playCamP = head camerasP --fromList $ camerasP!!0
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
        name' = view Prj.name project
        resX' = (unsafeCoerce $ view Prj.resx project) :: CInt
        resY' = (unsafeCoerce $ view Prj.resy project) :: CInt
