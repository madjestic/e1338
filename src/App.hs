{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}

module App
  ( App    (..)
  , AppRun (..)
  , Stage   (..)
  , Options (..)
  , options
  , App.name
  , App.resx
  , App.resy
  , App.objects
  , playCam
  , App.cameras
  , mainApp
  , appIntro
  , appPlay
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

data AppRun = Default
  deriving Show

data Stage =
     AppIntro
   | AppRun AppRun
   | AppFinished
   | AppMenu
  deriving Show

data App =
     App
     {
       _debug   :: (Double, Double)
     , _options :: Options
     , _gStg    :: Stage
     , _objects :: ObjectTree
     , _playCam :: Camera
     , _cameras :: [Camera]
     } deriving Show

data Options
   = Options
   { _name  :: String
   , _resx  :: CInt
   , _resy  :: CInt
   } deriving Show

$(makeLenses ''Options)
$(makeLenses ''App)

-- < App Logic > ---------------------------------------------------------

mainApp :: App -> App -> SF AppInput App
mainApp app0 app1 =
  loopPre app0 $
  proc (input, appState) -> do
    gs <- case _gStg appState of
            AppIntro -> appIntro            -< (input, appState)
            AppRun Default -> appPlay app0 app1 -< input
    returnA -< (gs, gs)

loadDelay = 10.0  :: Double -- make it into App options value                           

appIntro :: SF (AppInput, App) App
appIntro =
  switch sf cont
     where sf =
             proc (input, appState) -> do
               introState <- returnA -< appState
               playState  <- returnA -< appState { _gStg =  AppRun Default }
               skipE      <- keyInput SDL.ScancodeSpace "Pressed" -< input
               waitE      <- after loadDelay () -< ()
               returnA    -< (introState, (skipE `lMerge` waitE) $> playState)
           cont app  =
             proc input -> returnA -< app

appPlay :: App -> App -> SF AppInput App
appPlay intro app =
  switch sf (const (mainApp intro app))
     where sf =
             proc input -> do
               app'   <- updateApp app -< input
               reset   <- keyInput SDL.ScancodeSpace "Pressed" -< input
               returnA -< (app', reset $> app)

updateApp :: App -> SF AppInput App
updateApp app =
  proc input -> do
    --(cams, cam) <- updateCameras (App._cameras app, App._playCam app) -< input
    (cams, cam) <- updateCameras (App._cameras app, App._playCam app) -< (input, App._playCam app)

    objs        <- updateObjects        filteredNonGravityObjs -< ()
    let objsIntMap = IM.fromList (zip filteredNonGravityObjsIdxs objs)
    
    objs'       <- updateObjectsGravity filteredGravityObjs -< filteredGravityObjs
    let objs'IntMap = IM.fromList (zip filteredGravityObjsIdxs objs')

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
        
        filterGravityIntObjMap  = IM.filter (any (\case Gravity {} -> True; _ -> False) . view Object.solvers) intObjMap
        filteredGravityObjs     = snd <$> IM.toList filterGravityIntObjMap
        filteredGravityObjsIdxs = fst <$> IM.toList filterGravityIntObjMap

        filterNonGravityIntObjMap  = IM.filter (any (\case Gravity {} -> False; _ -> True) . view Object.solvers) intObjMap
        filteredNonGravityObjs     = snd <$> IM.toList filterNonGravityIntObjMap
        filteredNonGravityObjsIdxs = fst <$> IM.toList filterNonGravityIntObjMap

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
          --AppRun
          AppIntro
          objTree
          pCam
          cams

    print "finished initializing app resources..."
    return app
      where
        name' = view Prj.name project
        resX' = (unsafeCoerce $ view Prj.resx project) :: CInt
        resY' = (unsafeCoerce $ view Prj.resy project) :: CInt
