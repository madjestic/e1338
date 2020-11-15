{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings, Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where 

import Control.Concurrent
import Control.Lens
import Data.Text                 (pack)
import Foreign.C
import FRP.Yampa          hiding (identity)

import SDL                hiding ( Point
                                 , M44
                                 , M33
                                 , Event
                                 , Mouse
                                 , RenderDrivers
                                 , (^+^)
                                 , (*^)
                                 , _xyz)

import Graphics.Rendering.OpenGL ( PrimitiveMode(..))

import System.Environment       (getArgs)
import Unsafe.Coerce

import Game
import Object         as Obj
import Project        as Prj
import AppInput
import Rendering      as R

import Debug.Trace    as DT

-- -- < Animate > ------------------------------------------------------------
type WinInput = Event SDL.EventPayload
type WinOutput = (Game, Bool)

animate :: SDL.Window
        -> SF WinInput WinOutput  -- ^ signal function to animate
        -> IO ()
animate window sf =
  do
    reactimate (return NoEvent)
               senseInput
               renderOutput
               sf
    closeWindow window
    
      where
        senseInput _ =
          do
            lastInteraction <- newMVar =<< SDL.time
            currentTime <- SDL.time                          
            dt <- (currentTime -) <$> swapMVar lastInteraction currentTime --dtime
            mEvent <- SDL.pollEvent
            
            return (dt, Event . SDL.eventPayload <$> mEvent)
            
        renderOutput _ (game, shouldExit) =
          do
            lastInteraction <- newMVar =<< SDL.time
            -- currentTime <- SDL.time                          
            -- dt <- (currentTime -) <$> swapMVar lastInteraction currentTime --dtime
            -- putStrLn $ "FPS :" ++ show (0.0001/dt)
            -- TODO: send dt to renderer to display FPS in game
            
            R.render
              --dt
              lastInteraction
              R.OpenGL
              (BackendOptions { primitiveMode = Triangles})
              --(BackendOptions { primitiveMode = Points})
              window
              game
            return shouldExit

-- < Main Function > -----------------------------------------------------------
main :: IO ()
main = do

  args <- getArgs
  introProj <- Prj.parse (unsafeCoerce (args!!0) :: FilePath)
  proj      <- Prj.parse (unsafeCoerce (args!!1) :: FilePath)

  let title = pack $ view Prj.name proj
      resX  = (unsafeCoerce $ view Prj.resx proj) :: CInt
      resY  = (unsafeCoerce $ view Prj.resy proj) :: CInt

  window    <- openWindow
               title
               (resX, resY)

  -- | SDL Mouse Options
  -- setMouseLocationMode RelativeLocation

  print "Initializing Game"
  intro <- initGame initVAO introProj
  game  <- initGame initVAO proj
  
  print "Initializing Resources1"
  let fntObjs = concat $ toListOf (Game.objects . gui . Obj.fonts) game :: [Object]
      fgrObjs = concat $ toListOf (Game.objects . Obj.foreground)  game :: [Object]
      bgrObjs = concat $ toListOf (Game.objects . Obj.background)  game :: [Object]

  -- print $ "Init fntObjs :" ++ show fntObjs
  -- print $ "Init fgrObjs :" ++ show fgrObjs
  -- print $ "Init bgrObjs :" ++ show bgrObjs  

  _ <- bindTexureUniforms $ fgrObjs ++ fntObjs ++ bgrObjs
  --_ <- bindTexureUniforms $ view (Game.objects) game

  -- print $ "init Game :" ++ show game
  
  print "Starting Game."
  animate
    window
    (parseWinInput >>> (mainGame intro (game {_gStg = GamePlaying}) &&& handleExit))
  return ()
