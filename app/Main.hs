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

import App
import Object         as Obj
import Project        as Prj
import AppInput                 (parseWinInput) 
import Rendering      as R

import Debug.Trace    as DT

-- -- < Animate > ------------------------------------------------------------
type WinInput = Event SDL.EventPayload
type WinOutput = (App, Bool)

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

  let
  args <- getArgs
  introProj <- Prj.parse (unsafeCoerce (args!!0) :: FilePath)
  proj      <- Prj.parse (unsafeCoerce (args!!1) :: FilePath)
  
  let
    title   = pack $ view Prj.name proj
    resX    = (unsafeCoerce $ view Prj.resx proj) :: CInt
    resY    = (unsafeCoerce $ view Prj.resy proj) :: CInt
    --camMode = view Prj.camMode proj :: String

  window    <- openWindow
               title
               (resX, resY)

  -- | SDL Mouse Options
  let camMode =
        case view Prj.camMode proj of
          "RelativeLocation" -> RelativeLocation
          "AbsoluteLocation" -> AbsoluteLocation
          _ -> error "wrong mouse mode"

  setMouseLocationMode camMode

  putStrLn "\n Initializing App"
  intro <- initApp initVAO introProj
  app   <- initApp initVAO proj
  
  print "Initializing Resources"
  let fntObjs = concat $ toListOf (App.objects . gui . Obj.fonts) app :: [Object]
      fgrObjs = concat $ toListOf (App.objects . Obj.foreground)  app :: [Object]
      bgrObjs = concat $ toListOf (App.objects . Obj.background)  app :: [Object]

  _ <- bindTexureUniforms $ fgrObjs ++ fntObjs ++ bgrObjs
  
  print "Starting App."
  animate
    window
    (parseWinInput >>> (appRun intro (app {_interface = Main Default}) &&& handleExit))
  return ()    
