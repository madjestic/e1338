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
import AppInput                 (parseWinInput) 
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
            
            R.render
              lastInteraction
              R.OpenGL
              (BackendOptions { primitiveMode = Triangles})
              window
              game
            return shouldExit

-- < Main Function > -----------------------------------------------------------
main :: IO ()
main = do

  let
  args <- getArgs
  introProj <- Prj.parse (unsafeCoerce (args!!0) :: FilePath)
  proj      <- Prj.parse (unsafeCoerce (args!!1) :: FilePath)
  
  let
    title = pack $ view Prj.name proj
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

  _ <- bindTexureUniforms $ fgrObjs ++ fntObjs ++ bgrObjs
  
  print "Starting Game."
  animate
    window
    (parseWinInput >>> (mainGame intro (game {_gStg = GamePlaying}) &&& handleExit))
  return ()
