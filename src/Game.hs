{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Arrows #-}

module Game
  ( Game    (..)
  , Stage   (..) 
  , Options (..)
  , debug'
  , options
  , Game.name
  , Game.resx
  , Game.resy
  , Game.objects
  , pCamIDX
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
import FRP.Yampa --(SF, returnA, isEvent, (>>^), switch)
import Foreign.C                              (CInt)
import Linear.Matrix
import SDL.Input.Keyboard.Codes as SDL
import Unsafe.Coerce

import AppInput
import Camera
import Controllable
import Descriptor
import Material
import Object
import Project     as Prj
import Utils

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
     , _debug'  :: Int
     , _options :: Options
     , _gStg    :: Stage
     , _objects :: ObjectTree
     , _pCamIDX :: Int -- TODO: turn it into and index and look up a camera from the list, is integration in a SF (otherwise it does not work)
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
               --waitE      <-  now () -< ()
               returnA    -< (introState, (skipE `lMerge` waitE) $> playState)
               --returnA    -< (introState, (waitE) $> playState)
           cont game  = 
             proc input -> returnA -< game

gamePlay :: Game -> Game -> SF AppInput Game
gamePlay intro game =
  switch sf (const (mainGame intro game)) --cont       
     where sf =
             proc input -> do
               game'   <- updateGame game -< input
               --game'   <- updateGame (DT.trace ("gamePlay game :" ++ show game) $ game) -< input
               reset   <- keyInput SDL.ScancodeSpace "Pressed" -< input
               returnA -< (game', reset $> game)
           --cont = const mainGame game

-- updateGame :: Game -> SF AppInput Game
-- updateGame game = 
--   proc input -> do
--     -- gui  <- updateGUI     $ Game._gui       game -< ()
--     cam  <- updateCamera  $ Game._playCam              game  -< input
--     objs <- updateObjects $ _foreground (Game._objects game) -< ()

--     --returnA  -< set (Game.objects . foreground) (DT.trace ("updateGame objs :" ++ show objs)$ objs)            
--     returnA  -< set (Game.objects . foreground) objs
--               $ set Game.playCam cam
--               $ game

updateCamera' :: [Camera] -> Int -> SF (AppInput, Game) Camera
updateCamera' cams idx = 
  proc (input, game) ->
    do
      ctl' <- updateController (view controller (cams!!idx)) -< input -- add a switch for mouse moving / stopped, switch between integral and static
      let
        cam0 = cams!!idx
        cam' = cam0 { Camera._controller = ctl' }
      --returnA -< (DT.trace ("cam' :" ++ show cam') $ cam')
      returnA -< cam'

updateCamera'' :: [Camera] -> Int -> SF (AppInput, Int) Camera
updateCamera'' cams idx = 
  proc (input, idx) ->
    do
      ctl' <- updateController (view controller (cams!!idx)) -< input -- add a switch for mouse moving / stopped, switch between integral and static
      let
        cam0 = cams!!(idx `mod` 2)
        cam' = cam0 { Camera._controller = ctl' }
      --returnA -< (DT.trace ("cam' :" ++ show cam') $ cam')
      returnA -< cam'

updateCamera''' :: ([Camera], Camera) -> SF AppInput ([Camera], Camera)
updateCamera''' (cams0, cam0) =
  switch sf cont
  where
    sf =
      proc input -> do
        cams <- switchCameras cams0 -< ()
        cam  <- updateCamera  $ cams0!!0 -< input

        kev <- keyInput SDL.ScancodeX "Pressed" -< input

        let
          result  = (cams0, cam0)            
          result' = (cams,  cam)
          
        returnA -<
          ( result'
          , kev $> result' )
          
    cont = updateCamera'''

updateGame :: Game -> SF AppInput Game
updateGame game = 
  proc input -> do

    kev <- keyInput SDL.ScancodeC "Pressed" -< input
    idx  <- accumHoldBy (+) 0 -< (kev $> 1)
    -- idx <- ((_debug' game) +) ^<< integral <<< constant 1 -< ()
    -- cam  <- updateCamera  $ (Game._cameras game)!!(_pCamIDX game) -< input
    -- cam  <- updateCamera  $ (Game._cameras game)!!1 -< input
    (cams, cam) <- updateCamera''' ((Game._cameras game), (Game._playCam game)) -< input
    -- cam  <- updateCamera  $ (rotateList (_debug' game) (Game._cameras game))!!0 -< input
    -- cam  <- updateCamera'' (Game._cameras game) (_pCamIDX game) -< (input, idx)
    -- cam  <- updateCamera' (Game._cameras game) (_pCamIDX game) -< (input, game)
    -- cams <- switchCameras (Game._cameras game) -< ()
    objs <- updateObjects $ _foreground (Game._objects game) -< ()

    let
      objTree = Game._objects game
      result =
        game { _debug'       = idx -- (DT.trace ("idx :" ++ show idx) $ idx)
             , Game._pCamIDX = idx `mod` 2
             , Game._objects = (objTree {_foreground = objs})
             , Game._cameras = cams
             , _playCam      = cam
             --, _playCam      = (Game._cameras game)!!(idx `mod` 2)
             }

    returnA  -< (DT.trace (  "_debug' :"  ++ show (_debug'  result) ++ "\n"
--                          ++ "_playCam :" ++ show ( cam )           ++ "\n"
                          ++ "_pCamIDX :" ++ show (_pCamIDX result) ++ "\n") $ result)
    -- returnA  -< result --game {_debug' = (DT.trace ("idx :" ++ show idx) $ idx)}
      --   set (Game._debug') (DT.trace ("idx :" ++ show idx) $ idx)
      -- $ set (Game.objects . foreground) objs
      -- $ set Game.pCamIDX 1 --(DT.trace ("idx :" ++ show idx) $ idx)
      -- $ set Game.playCam cam
      -- $ game

-- updateGame'1 :: Game -> SF AppInput Game
-- updateGame'1 game = 
--   proc input -> do
--     -- cams <- switchCameras (Game._cameras game)     -< ()
--     -- cam  <- updateCamera  $ (Game._cameras game)!!0 -< input
--     (cams, cam) <- updateCameras ((Game._cameras game), (Game._playCam game)) -< input
--     objs <- updateObjects $ _foreground (Game._objects game) -< ()

--     --let result = game { Game._cameras = (DT.trace ("updateGame' cams :" ++ show cams )$ cams) }

--     --returnA  -< set (Game.objects . foreground) (DT.trace ("updateGame objs :" ++ show objs)$ objs)            
--     returnA  -< -- result -- game { Game._cameras = (DT.trace ("updateGame' cams :" ++ show cams )$ cams) }
--         set (Game.objects . foreground) objs
--       $ set Game.playCam cam -- (DT.trace ("updateGame' cam  :" ++ show cam )$ cam)
--       $ set Game.cameras cams -- (DT.trace ("updateGame' cams :" ++ show cams )$ cams)
--       $ game

-- updateGame'' :: Game -> SF AppInput Game
-- updateGame'' game =
--   switch sf cont
--   where
--     sf =
--       proc input -> do
--         cams <- switchCameras (Game._cameras game) -< ()
--         cam  <- updateCamera  $ (Game._cameras game)!!0 -< input
--         objs <- updateObjects $ _foreground (Game._objects game) -< ()

--         kev <- keyInput SDL.ScancodeC "Pressed" -< input

--         let
--           result
--             = set (Game.objects . foreground) objs
--             $ set Game.playCam cam
--             $ game
            
--           result'
--             = set (Game.objects . foreground) objs
--             $ set Game.playCam cam
--             $ set Game.cameras cams                        
--             $ game
        
--         returnA -<
--           ( result
--           , kev $> result' )
          
--     cont = updateGame''

-- updateGame''' :: Game -> SF AppInput Game
-- updateGame''' game = 
--   proc input -> do
--     -- gui  <- updateGUI     $ Game._gui       game -< ()
--     objs <- updateObjects $ _foreground (Game._objects game) -< ()
--     cams <- switchCameras (Game._cameras game) -< ()
--     cam  <- updateCamera  $ (Game._cameras game)!!0 -< input

--     -- (kbrd',  kevs) <- updateKeyboard (view (playCam . controller) game)
--     --                -< input
--     kev <- keyInput SDL.ScancodeC "Pressed" -< input

--     let
--       --cam' = undefined :: Camera
--       --objTree = Game._objects game
--       result -- = set Game.playCam cam $ game
--         = set (Game.objects . foreground) objs
--         $ set Game.playCam cam
--         $ game
--         -- = game { Game._objects = (objTree {_foreground = objs}) }
--       result' -- = set Game.cameras cams $ game
--         = set (Game.objects . foreground) objs
--         $ set Game.playCam cam
--         $ set Game.cameras cams                        
--         $ game
--         -- = game { Game._objects = (objTree {_foreground = objs}) }
    
--     returnA -< result'
--       -- ( result
--       -- , kev $> result' )
    
handleExit :: SF AppInput Bool
handleExit = quitEvent >>^ isEvent

centerView :: SF AppInput Bool
centerView = centerEvent >>^ isEvent

-- -- < Init Game State > ------------------------------------------------------

-- TODO: initGame could be loaded from a file, similar to "Save Game" feature.
initGame ::
     (([Int], Int, [Float], Material) -> IO Descriptor)
  -> Project
  -> IO Game
initGame initVAO project =
  do
    print "initializing game resources..."
    print $ "project name :" ++ (view Prj.name project)
    objs  <- (loadObjects initVAO project)

    let 
        camPs    = fmap fromList $ view Prj.cameras project
        playCamP = camPs!!0 --fromList $ camPs!!0
    --pc <- fromVGeo $ fromPGeo pCloud  -- PCloud Point Cloud
    --let objs = [pc]
    let game =
          Game
          (-42,-17)
          0
          ( Options
            name'
            resX'
            resY'
          )
          --GamePlaying
          GameIntro
          --(DT.trace ("initGame.objs :" ++ show objs) $ objs)
          objs
          0
          (set controller (defaultCamController { Controllable._transform = playCamP }) defaultCam)                     -- | :: Camera   - player Camera
          (fmap (\camP -> (set controller (defaultCamController { Controllable._transform = camP }) defaultCam)) camPs) -- | :: [Camera] - list of Cameras
    print "finished initializing game resources..."          
    return game
      where        
        name' = view Prj.name project
        resX' = (unsafeCoerce $ view Prj.resx project) :: CInt
        resY' = (unsafeCoerce $ view Prj.resy project) :: CInt
        -- pcam  = undefined :: Camera
        -- camPs  = undefined :: [Camera]

loadFonts :: IO [Object]
loadFonts =
  do
    return undefined
