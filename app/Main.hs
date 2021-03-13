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
              (fromApplication app)
            return shouldExit

-- < Main Function > -----------------------------------------------------------
main :: IO ()
main = do

  -- let argsDebug = return ["./projects/intro_XXII", "./projects/intro_XXII"]
  let argsDebug = return ["./projects/test", "./projects/test"]
  args <- if debug then argsDebug else getArgs

  -- TODO: introduce propper CLI args parsing, as in:
  
  -- parseArgs :: [String] -> IO String
  -- parseArgs ["-h"] = help    >> exit
  -- parseArgs ["-v"] = version >> exit
  -- parseArgs []     = getContents
  -- parseArgs fs     = putStrLn ("(re)Generating UUIDs for  project file: " ++ show (head fs)) >> return (concat fs)
   
  -- help    = putStrLn "Usage: genUUID [-- -vh] [file ..]"
  -- version = putStrLn "genUUID 0.1"
  -- exit    = exitWith ExitSuccess
  -- die     = exitWith (ExitFailure 1)
  
  introProj <- P.parse (unsafeCoerce (args!!0) :: FilePath)
  mainProj  <- P.parse (unsafeCoerce (args!!1) :: FilePath)
  
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

  let app =
        Application
        { _interface = Intro
        , _intro = intro
        , _main  = main }
  
  print "Binding Texture Uniforms"
  let
    fgrObjs'= concat $ toListOf (App.objects . O.foreground)  intro :: [Object]
    fntObjs = concat $ toListOf (App.objects . gui . O.fonts) main  :: [Object]
    fgrObjs = concat $ toListOf (App.objects . O.foreground)  main  :: [Object]
    bgrObjs = concat $ toListOf (App.objects . O.background)  main  :: [Object]

  _ <- bindTexureUniforms $ fgrObjs' ++ fgrObjs ++ fntObjs ++ bgrObjs
  
  print "Starting App."
  animate
    window
    (parseWinInput >>> appRun app &&& handleExit)
  return ()    
