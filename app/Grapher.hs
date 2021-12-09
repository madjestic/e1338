{-# LANGUAGE CPP    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Main where

import Control.Monad
import Control.Concurrent
import Data.Massiv.Array as A hiding (windowSize, mapM_, zip)
import Data.Massiv.Array.Unsafe  as AU
import Data.Massiv.Array.Mutable as AM
import Data.Word                (Word8)
import Graphics.UI.GLUT as GLUT (getArgsAndInitialize, mainLoop)
import System.Exit              (ExitCode(..), exitSuccess, exitWith)
import System.Environment       (getArgs)
import Text.Read                (readMaybe)

import System.Random.SplitMix as SplitMix
import Foreign.C                (CInt(CInt))
import Data.Text                (Text, pack, chunksOf)
import Data.List.Split        as DLS (chunksOf)
import Graphics.GLUtil
import Graphics.Rendering.OpenGL as GL
import Unsafe.Coerce

import Control.Lens.Fold        (toListOf)
import Data.UUID                (nil)
import Data.UUID.V1             (nextUUID)
import Data.Maybe               (fromMaybe)

import FRP.Yampa as FRP hiding (identity)
import SDL              hiding ( Point
                               , M44
                               , M33
                               , Event
                               , Mouse
                               , RenderDrivers
                               , (^+^)
                               , (*^)
                               , _xyz)
import Control.Lens (view)

import Rendering as R
import Application
import App
import AppInput                 (parseWinInput)
import Object as O
import Project        as P hiding (PreObject)
import Graph

-- for testing like in Main, remove later
import qualified Texture  as T (uuid)
import qualified Material as M (textures)

import Debug.Trace as DT

debug = True

graphRules :: Word8 -> Word8 -> Word8
graphRules 0 3 = 1
graphRules 1 2 = 1
graphRules 1 3 = 1
graphRules _ _ = 0

graphStencil :: Stencil Ix2 Word8 Word8
graphStencil = makeStencil (Sz (3 :. 3)) (1 :. 1) $ \ f ->
  graphRules (f (0 :. 0))
  (f (-1 :. -1) + f (-1 :. 0) + f (-1 :. 1) +
   f ( 0 :. -1) +               f ( 0 :. 1) +
   f ( 1 :. -1) + f ( 1 :. 0) + f ( 1 :. 1))

graph :: Array S Ix2 Word8 -> Array S Ix2 Word8
graph = compute . A.mapStencil Wrap graphStencil

initLife :: Sz2 -> Array S Ix2 Word8 -> Array S Ix2 Word8
initLife sz arr =
  compute $
  insertWindow
    (makeArrayR D Par sz (const 0))
    (Window ix0 (size arr) (index' arr . subtract ix0) Nothing)
  where
    ix0 = liftIndex (`div` 2) (unSz (sz - size arr))

-- blinker :: Array S Ix2 Word8
-- blinker = [ [0, 1, 0]
--           , [0, 1, 0]
--           , [0, 1, 0] ]


-- glider :: Array S Ix2 Word8
-- glider = [ [0, 1, 0]
--          , [0, 0, 1]
--          , [1, 1, 1] ]

inf :: Array S Ix2 Word8
inf =
  [ [1, 1, 1, 0, 1]
  , [1, 0, 0, 0, 0]
  , [0, 0, 0, 1, 1]
  , [0, 1, 1, 0, 1]
  , [1, 0, 1, 0, 1] ]

genArray :: Int -> Int -> IO (Array S Ix2 Word8)
genArray m n = do
    let lol = unsafeCoerce $ DLS.chunksOf n [ x | x <- fmap (`div`4) [0..(4*m*n-1)]] :: [[Word8]] -- lol = list of lists
    --let lol = unsafeCoerce $ [ x | x <- [0..(m*n-1)]] :: [[Word8]] -- lol = list of lists
    fromListsM Seq lol :: IO (Array S Ix2 Word8)

genArray' :: Int -> Int -> IO (Array S Ix2 Int)
genArray' m n = do
    let lol = unsafeCoerce $ DLS.chunksOf m [ x | x <- [0..(m*n-1)]] :: [[Int]] -- lol = list of lists
    fromListsM Seq lol :: IO (Array S Ix2 Int)

genArray'' :: Int -> Int -> IO (Array S Ix2 Word8)
genArray'' m n = do
    let lol = unsafeCoerce $ DLS.chunksOf m [ x | x <- [0..(m*n-1)]] :: [[Int]] -- lol = list of lists
    x <- fromListsM Seq lol :: IO (Array S Ix2 Int)
    let
      --x'  = A.map (toEnum . flip mod 2 . abs) x :: Array D Ix2 Word8
      x'  = A.map (toEnum . const 1) x :: Array D Ix2 Word8
      x'' = convert x' :: Array S Ix2 Word8
    return x''

inf2 :: Array S Ix2 Word8
inf2 =
  [ [1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1]
  , [1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0]
  , [0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1]
  , [0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1]
  , [1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1]
  , [1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0]
  , [0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1]
  , [0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1]
  , [1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1]
  , [1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0]
  , [0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1]
  , [0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1]
  , [1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1]
  , [1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0]
  , [0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1]
  , [0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1]
  , [1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1]
  , [1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0]
  , [0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1]
  , [0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1]
  , [1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1]
  , [1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0]
  , [0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1]
  , [0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1]
  , [1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1]
  , [1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0]
  , [0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1]
  , [0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1]
  , [1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1]
  , [1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0]
  , [0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1]
  , [0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1]
  , [1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1]
  , [1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0]
  , [0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1]
  , [0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1]
  , [1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1]
  , [1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0]
  , [0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1]
  , [0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1]
  , [1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1]
  ]

rand :: Int -> Int -> Array S Ix2 Word8
rand m n = r''
  where
    r   = snd $ randomArrayS gen (Sz2 m n) SplitMix.nextInt :: Array S Ix2 Int --(ParN 2) (Sz2 5 5)
    r'  = A.map (toEnum . flip mod 2 . abs) r :: Array D Ix2 Word8
    r'' = convert r'     :: Array S Ix2 Word8
    gen = SplitMix.mkSMGen 0

type WinInput  = FRP.Event SDL.EventPayload
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

            return (dt, FRP.Event . SDL.eventPayload <$> mEvent)

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

updateArray :: MArray RealWorld S Ix2 Word8 -> Array S Ix2 Word8 -> IO (Array S Ix2 Word8)
updateArray mArr arr = do
  return arr

initGraph :: Sz2 -> Array S Ix2 Word8 -> Array S Ix2 Word8
initGraph sz arr =
  -- compute $ makeArrayR D Par sz (const 1)
  -- compute $ makeArrayR D Par sz (const 1)
  arr

initResources :: Application -> Graph -> IO Application
initResources app0 grph = do
    uuid <- nextUUID
    let
      objs = introObjs ++ fntObjs ++ fgrObjs ++ bgrObjs
      -- txs  = concat $ concatMap (toListOf (materials . traverse . M.textures)) objs -- :: [Texture]
      hmap = [(fromMaybe nil uuid, 0::GLuint)]

    putStrLn "Initializing Resources..."
    putStrLn "Generating Textures..."
    -- mapM_ (bindTextureObject hmap) txs
    -- generate and bind texture:
    -- tex <- genTex (view sz grph)
    putStrLn $ "texture size : " ++ show (view sz grph)
    texObj <- genTexObject 0 grph
    --texObj <- loadTex "textures/checkerboard.png"
    _ <- DT.trace ("texObj : " ++ show texObj) $ return ()
    bindTextureObject (0::GLuint) texObj
    putStrLn "Finished loading textures."

    return app0 { _hmap = hmap }
      where
        introObjs = concat $ toListOf (App.objects . O.foreground)  (_intro app0) :: [Object]
        fntObjs   = concat $ toListOf (App.objects . gui . O.fonts) (_main app0)  :: [Object]
        fgrObjs   = concat $ toListOf (App.objects . O.foreground)  (_main app0)  :: [Object]
        bgrObjs   = concat $ toListOf (App.objects . O.background)  (_main app0)  :: [Object]

-- initResources :: Application -> IO Application
-- initResources app0 =
--   do
--     let
--       objs = introObjs ++ fntObjs ++ fgrObjs ++ bgrObjs
--       txs  = concat $ concatMap (toListOf (materials . traverse . M.textures)) objs -- :: [Texture]
--       uuids = fmap (view T.uuid) txs
--       hmap = zip uuids [0..]

--     putStrLn "Initializing Resources..."
--     putStrLn "Loading Textures..."
--     mapM_ (bindTexture hmap) txs
--     putStrLn "Finished loading textures."

--     return app0 { _hmap = hmap }
--       where
--         introObjs = concat $ toListOf (App.objects . O.foreground)  (_intro app0) :: [Object]
--         fntObjs   = concat $ toListOf (App.objects . gui . O.fonts) (_main app0)  :: [Object]
--         fgrObjs   = concat $ toListOf (App.objects . O.foreground)  (_main app0)  :: [Object]
--         bgrObjs   = concat $ toListOf (App.objects . O.background)  (_main app0)  :: [Object]

main :: IO ()
main = do
  -- let helpTxt =
  --       "Usage:\n  --               \   life [WIDTH HEIGHT] [SCALE]\n  --               \ * WIDTH - number of cells horizontally (default 100)\n  --               \ * HEIGHT - number of cells vertically (default 70)\n  --               \ * SCALE - scaling factor, or how many pixels one cell should take on a screen\n  --               \ example: \n  --               \ cabal run exe:GameOfLife -- 35 35\n"
  -- (_progName, args) <- getArgsAndInitialize
  -- when (args == ["--help"]) $ putStrLn helpTxt >> exitSuccess
  -- (m, n, s) <- case fmap readMaybe args of
  --   [Just m, Just n, Just s]
  --     | m > 0 && n > 0 && s > 0 -> return (m, n, s)
  --   [Just m, Just n]
  --     | m > 0 && n > 0 -> return (m, n, 10)
  --   [] -> return (41, 50, 100)
  --   --[] -> return (128, 128, 10)
  --   _ -> do
  --     putStrLn "Invalid arguments."
  --     putStrLn helpTxt
  --     exitWith $ ExitFailure 1

  let argsDebug = return ["./projects/intro", "./projects/view_model"]
  args <- if debug then argsDebug else getArgs

  introProj <- P.read (unsafeCoerce (args!!0) :: FilePath)
  mainProj  <- P.read (unsafeCoerce (args!!1) :: FilePath)

  let
    -- title = "Game of Life" :: String
    title = pack $ view P.name mainProj -- "Game of Life" :: String

    resx  = view P.resx mainProj
    resy  = view P.resy mainProj
    resX  = unsafeCoerce resx :: CInt -- unsafeCoerce m :: CInt
    resY  = unsafeCoerce resy :: CInt --unsafeCoerce n :: CInt
    opts  = BackendOptions
            { primitiveMode = Triangles
            , bgrColor      = Color4 0.0 0.0 0.0 1.0
            , ptSize        = 3.0
            }
    s      = 0 :: Int
    -- sz     = Sz2 m n
    sz     = Sz2 resx resy
    --graph  = rand resx resy
    --graph  = rand 64 64
  graph  <- genArray resx resy
  --graph  <- genArray 1024 256 -- <- Somewhere here...
  --graph  <- genArray 900 900
  let
    -- graphArray = initGraph sz graph --inf2 -- func
    -- wSz    = size (pixelGrid s graphArray)
    -- wSz = sz
  print $ "sz :" ++ show sz

  -- init a Mutable Array for storing graph data
  -- mArr   <- newMArray' wSz :: IO (MArray RealWorld S Ix2 Word8)
  mArr   <- newMArray sz 1 :: IO (MArray RealWorld S Ix2 Word8)
  window <- openWindow
            title
            (resX, resY)

  let g = Graph sz graph mArr
      s = 1 :: Int
  -- drawGraph s g
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

  app <- initResources initApp g
  --app <- initResources initApp

  putStrLn "Starting App."
  animate
    window
    (parseWinInput >>> appRun app &&& handleExit)
  return ()

  -- return ()
  -- mainLoop opts window s sz mArr graphArray
  -- where
  --   mainLoop opts window s sz mArr arr = do
  --     -- arr'  <- updateArray mArr arr
  --     render opts window s sz mArr arr
  --     mainLoop opts window s sz mArr arr

--updateArray :: Int -> MArray RealWorld S Ix2 Word8 -> Array S Ix2 Word8 -> IO (Array S Ix2 Word8)
--updateArray s mArr arr = do
