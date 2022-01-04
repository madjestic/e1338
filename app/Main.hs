{-# LANGUAGE CPP    #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings, Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Main where 

import Control.Concurrent ( swapMVar, newMVar )
import Control.Lens       ( toListOf, view )
import Data.Set           ( fromList, toList )
import Data.Text          ( pack)
import Foreign.C          ( CInt )
import FRP.Yampa as FRP   ( (>>>), reactimate, Arrow((&&&)), Event(..), SF )
import SDL
    ( pollEvent,
      setMouseLocationMode,
      time,
      Event(eventPayload),
      EventPayload,
      LocationMode(AbsoluteLocation, RelativeLocation),
      Window )
import Graphics.Rendering.OpenGL ( PrimitiveMode(..), Color4 (Color4), pointSize)
import System.Environment        ( getArgs )
import Unsafe.Coerce             ( unsafeCoerce )

import Application
    ( Application(Application, _intro, _main, _hmap),
      Interface(Intro))
import Update (appRun)    
import App ( objects, initApp )
import Update (handleExit)
import Object as O
    ( Object, materials, fonts, background, foreground, gui )
import Project as P ( camMode, resy, resx, name, read )
import AppInput                  ( parseWinInput ) 
import Rendering as R
    ( BackendOptions(BackendOptions, primitiveMode, bgrColor, ptSize),
      Backend(OpenGL),
      openWindow,
      closeWindow,
      render,
      initResources,
      bindTexture,
      initVAO )
import qualified Material as M
import qualified Texture  as T

import Debug.Trace    as DT

#ifdef DEBUG
debug = True
#else
debug = False
#endif

-- -- < Animate > ------------------------------------------------------------
type WinInput = FRP.Event SDL.EventPayload
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
              R.OpenGL (
              BackendOptions
               { primitiveMode = Triangles
               , bgrColor      = Color4 0.0 0.0 0.0 1.0
               , ptSize        = 3.0
               })
              window
              app
            return shouldExit

-- < Main Function > -----------------------------------------------------------
main :: IO ()
main = do

  --let argsDebug = return ["./projects/intro", "./projects/view_model"]
  let argsDebug = return ["./projects/intro_XXII", "./projects/solar_system"]
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
