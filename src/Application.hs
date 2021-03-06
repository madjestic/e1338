{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}

module Application
  ( Application (..)
  , Main        (..)
  , Interface   (..)
  , appRun
  , appIntro
  , fromApplication
  ) where

import Control.Lens
import Data.Functor              (($>))
import SDL.Input.Keyboard.Codes as SDL

import App
import AppInput
import FRP.Yampa

import Debug.Trace as DT

data Main = Default
  deriving Show

data Interface =
     Intro
   | Main Main
   | Finished
   | Menu
  deriving Show

data Application
  = Application
  {
    _interface :: Interface
  , _intro     :: App
  , _main      :: App
  } deriving Show
$(makeLenses ''Application)

fromApplication :: Application -> App
fromApplication app =
  case (view interface app) of
    Intro        -> view intro app
    Main Default -> view main  app

appRun :: Application -> SF AppInput Application
appRun app =
  loopPre app $
  proc (input, appState) -> do
    as <- case _interface appState of
            Intro        -> appIntro -< (input, appState)
            Main Default -> appMain app { _interface =  Main Default } -< input
    returnA -< (as, as)

appIntro :: SF (AppInput, Application) Application
appIntro = 
  switch sf cont
     where sf =
             proc (input, appState) -> do
               introState <- returnA -< appState
               mainState  <- returnA -< appState { _interface =  Main Default }
               skipE      <- keyInput SDL.ScancodeSpace "Pressed" -< input
               waitE      <- after 5.0 () -< ()
               returnA    -< (introState, (skipE `lMerge` waitE) $> mainState)
           cont app  =
             proc input -> returnA -< app

appMain :: Application -> SF AppInput Application
appMain app = 
  switch sf cont
     where sf =
             proc input -> do
               app'        <- updateApp (fromApplication app) -< input
               reset       <- keyInput SDL.ScancodeSpace "Pressed" -< input

               let result = app { _main = app' }
               returnA     -< (result, reset $> app)
               
           cont = appRun    

