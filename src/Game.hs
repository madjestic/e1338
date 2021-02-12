{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}

module Game
  ( Game    (..)
  , Stage   (..)
  , Options (..)
  , options
  , Game.name
  , Game.resx
  , Game.resy
  , Game.objects
  , playCam
  , Game.cameras
  , mainGame
  , gameIntro
  , gamePlay
  , updateGame
  , handleExit
  , centerView
  , initGame
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

data Stage =
     GameIntro
   | GamePlaying
   | GameFinished
   | GameMenu
   deriving Show

data Game =
     Game
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
$(makeLenses ''Game)

-- < Game Logic > ---------------------------------------------------------

mainGame :: Game -> Game -> SF AppInput Game
mainGame game0 game1 =
  loopPre game0 $
  proc (input, gameState) -> do
    gs <- case _gStg gameState of
            GameIntro   -> gameIntro            -< (input, gameState)
            GamePlaying -> gamePlay game0 game1 -< input
    returnA -< (gs, gs)

loadDelay = 10.0  :: Double -- make it into Game options value                           

gameIntro :: SF (AppInput, Game) Game
gameIntro =
  switch sf cont
     where sf =
             proc (input, gameState) -> do
               introState <- returnA -< gameState
               playState  <- returnA -< gameState { _gStg =  GamePlaying }
               skipE      <- keyInput SDL.ScancodeSpace "Pressed" -< input
               waitE      <- after loadDelay () -< ()
               returnA    -< (introState, (skipE `lMerge` waitE) $> playState)
           cont game  =
             proc input -> returnA -< game

gamePlay :: Game -> Game -> SF AppInput Game
gamePlay intro game =
  switch sf (const (mainGame intro game))
     where sf =
             proc input -> do
               game'   <- updateGame game -< input
               reset   <- keyInput SDL.ScancodeSpace "Pressed" -< input
               returnA -< (game', reset $> game)

updateGame :: Game -> SF AppInput Game
updateGame game =
  proc input -> do
    (cams, cam) <- updateCameras (Game._cameras game, Game._playCam game) -< input

    objs        <- updateObjects        filteredNonGravityObjs -< ()
    let objsIntMap = IM.fromList (zip filteredNonGravityObjsIdxs objs)
    
    objs'       <- updateObjectsGravity filteredGravityObjs -< filteredGravityObjs
    let objs'IntMap = IM.fromList (zip filteredGravityObjsIdxs objs')

    let
      unionObjs = IM.union objs'IntMap objsIntMap
      objTree = Game._objects game
      result =
        game { Game._objects = (objTree {_foreground = snd <$> IM.toList unionObjs})
             , Game._cameras = cams
             , _playCam      = cam
             }

    returnA  -< result
      where
        idxObjs    = DL.indexed $ _foreground (Game._objects game)
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


-- -- < Init Game State > ------------------------------------------------------

initGame ::
     (([Int], Int, [Float], Material) -> IO Descriptor)
  -> Project
  -> IO Game
initGame initVAO project =
  do
    print   "initializing game resources..."
    print $ "project name :" ++ view Prj.name project
    objTree <- initObjectTree initVAO project

    let
      cams = fromProjectCamera <$> view Prj.cameras project
      pCam = head cams
      camerasP = Utils.fromList <$> toListOf (Prj.cameras . traverse . pTransform) project
      playCamP = head camerasP --fromList $ camerasP!!0
    --pc <- fromVGeo $ fromPGeo pCloud  -- PCloud Point Cloud
    --let objTree = [pc]
    let game =
          Game
          (-42,-17)
          ( Options
            name'
            resX'
            resY'
          )
          --GamePlaying
          GameIntro
          objTree
          pCam
          cams

    print "finished initializing game resources..."
    return game
      where
        name' = view Prj.name project
        resX' = (unsafeCoerce $ view Prj.resx project) :: CInt
        resY' = (unsafeCoerce $ view Prj.resy project) :: CInt
