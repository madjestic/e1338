{-# LANGUAGE CPP    #-}
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

import Application
import App
import Object         as O
import Project        as P
import AppInput                 (parseWinInput) 
import Rendering      as R
import Utils

import Debug.Trace    as DT

#ifdef DEBUG
debug = True
#else
debug = False
#endif

-- -- < Animate > ------------------------------------------------------------
type WinInput = Event SDL.EventPayload
type WinOutput = (Application, Bool)

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
            
        renderOutput _ (app, shouldExit) =
          do
            lastInteraction <- newMVar =<< SDL.time
            
            R.render
              lastInteraction
              R.OpenGL
              (BackendOptions { primitiveMode = Triangles})
              window
              app
            return shouldExit

-- < Main Function > -----------------------------------------------------------
main :: IO ()
main = do

  let argsDebug = return ["./projects/intro", "./projects/test"]
  args <- if debug then argsDebug else getArgs

  introProj <- P.read (unsafeCoerce (args!!0) :: FilePath)
  mainProj  <- P.read (unsafeCoerce (args!!1) :: FilePath)
  
  let
    title   = pack $ view P.name mainProj
    resX    = (unsafeCoerce $ view P.resx mainProj) :: CInt
    resY    = (unsafeCoerce $ view P.resy mainProj) :: CInt

  window    <- openWindow
               title
               (resX, resY)

  -- | SDL Mouse Options
  let camMode =
        case view P.camMode mainProj of
          "RelativeLocation" -> RelativeLocation
          "AbsoluteLocation" -> AbsoluteLocation
          _ -> error "wrong mouse mode"

  setMouseLocationMode camMode

  putStrLn "\n Initializing App"
  intro <- initApp initVAO introProj
  main  <- initApp initVAO mainProj

  let
    initApp =
      Application
      Intro
      intro
      main
      []

  app <- initResources initApp
  
  putStrLn "Starting App."
  animate
    window
    (parseWinInput >>> appRun app &&& handleExit)
  return ()    
