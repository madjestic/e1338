{-# LANGUAGE CPP    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Main where

import Foreign.C          ( CInt )
import Data.Text          ( pack)
import Control.Concurrent
import Data.Set (fromList, toList)
import Data.Massiv.Array.Manifest as AM (toByteString)
import Data.Massiv.Array as A hiding (tail, windowSize, mapM_, mapM, zip, fromList, toList, replicate)

import Data.Word                (Word8)
import System.Environment       (getArgs)
import Linear.Matrix
import System.Random.SplitMix as SplitMix
import Data.List.Split        as DLS (chunksOf)
import Graphics.Rendering.OpenGL as GL
import Unsafe.Coerce
import Control.Lens.Fold        (toListOf)

import FRP.Yampa as FRP hiding  (identity)
import SDL              hiding  ( Point
                                , M44
                                , M33
                                , Event
                                , Mouse
                                , RenderDrivers
                                , (^+^)
                                , (*^)
                                , _xyz
                                , Texture )

import Control.Lens             ( view )
import Graphics.GLUtil.Textures               (loadTexture, texInfo, texture2DWrap, TexColor(TexRGBA))

import Rendering as R
import Application
import App
import AppInput                 (parseWinInput)
import Object as O
import Project        as P hiding (PreObject)
import Graph
import Texture                 (uuid, name, Texture)
import Material as M (name)
import Material (Material, textures)
import Drawable
import Camera
import Controllable
import Descriptor
import Mouse
import Update                  (handleExit, appRun)
import Utils                   ((<$.>), (<*.>))

-- import Debug.Trace as DT

debug :: Bool
#ifdef DEBUG
debug = True
#else
debug = False
#endif

-- < Graph data > -------------------------------------------------------------------------------
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
initLife sz' arr' =
  compute $
  insertWindow
    (makeArrayR D Par sz' (const 0))
    (Window ix0 (size arr') (index' arr' . subtract ix0) Nothing)
  where
    ix0 = liftIndex (`div` 2) (unSz (sz' - size arr'))

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

-- < FRP Loop > ---------------------------------------------------------------------------------

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

        renderOutput _ (app', shouldExit) =
          do
            lastInteraction <- newMVar =<< SDL.time

            output lastInteraction window app'
            return shouldExit

toDrawable :: App -> [Object] -> Double -> [Drawable]
toDrawable app' objs time' = drs -- (drs, drs')
  where
    mpos = unsafeCoerce $ view (playCam . controller . device' . mouse . pos) app' -- :: (Double, Double)
    resX = fromEnum $ view (options . App.resx) app' :: Int
    resY = fromEnum $ view (options . App.resy) app' :: Int
    res  = (toEnum resX, toEnum resY) :: (CInt, CInt)
    cam  = view playCam app' :: Camera
    drs  = concatMap (toDrawable' mpos time' res cam) objs :: [Drawable]

toDrawable' :: (Double, Double) -> Double -> (CInt, CInt) -> Camera -> Object -> [Drawable]
toDrawable' mpos time' res cam obj = drs
  where
    drs      =
      (\u_mats' u_prog' u_mouse' u_time' u_res' u_cam' u_cam_a' u_cam_f' u_xform' ds' ps' name'
        -> Drawable name' (Uniforms u_mats' u_prog' u_mouse' u_time' u_res' u_cam' u_cam_a' u_cam_f' u_xform') ds' ps')
      <$.> mats <*.> progs <*.> mpos_ <*.> time_ <*.> res_ <*.> cam_ <*.> cam_a_ <*.> cam_f_ <*.> xforms <*.> ds <*.> progs <*.> names

    n      = length $ view descriptors obj:: Int
    mpos_  = replicate n mpos :: [(Double, Double)]
    time_  = replicate n time' :: [Double]
    res_   = replicate n res  :: [(CInt, CInt)]
    cam_   = replicate n $ view (controller . Controllable.transform) cam  :: [M44 Double]
    cam_a_ = replicate n $ _apt cam :: [Double]
    cam_f_ = replicate n $ _foc cam :: [Double]

    names  = toListOf (O.materials . traverse . M.name) obj :: [String]
    mats   = view O.materials   obj :: [Material]
    progs  = view O.programs    obj :: [Program]
    xforms = concat $ replicate n $ view O.transforms obj :: [M44 Double]
    ds     = view O.descriptors obj :: [Descriptor]

output :: MVar Double -> SDL.Window -> Application -> IO ()
output lastInteraction window application = do

  ticks'   <- SDL.ticks
  let currentTime = fromInteger (unsafeCoerce ticks' :: Integer) :: Double

  -- currentTime <- SDL.time
  -- dt <- (currentTime -) <$> readMVar lastInteraction

  let
    app'    = fromApplication application
    
    fntObjs = concat $ toListOf (App.objects . gui . O.fonts) app' :: [Object]
    fgrObjs = concat $ toListOf (App.objects . O.foreground)  app' :: [Object]
    -- bgrObjs = concat $ toListOf (App.objects . O.background)  app :: [Object]

    fntsDrs = toDrawable app' fntObjs currentTime :: [Drawable]
    objsDrs = toDrawable app' fgrObjs currentTime :: [Drawable]
    -- bgrsDrs = toDrawable app bgrObjs currentTime :: [Drawable]

    txs  = concat $ toListOf ( traverse . materials . traverse . textures) (fgrObjs ++ fntObjs) :: [Texture]
    hmap = _hmap application

    opts =
      BackendOptions
      { primitiveMode = Triangles
      , bgrColor      = Color4 0.0 0.0 0.0 1.0
      , ptSize        = 1.0 }
    
  clearColor $= bgrColor opts --Color4 0.0 0.0 0.0 1.0
  GL.clear [ColorBuffer, DepthBuffer]
  mapM_ (render txs hmap (opts { primitiveMode = Triangles })) objsDrs
  
  currentTime' <- SDL.time
  dt <- (currentTime' -) <$> readMVar lastInteraction
  renderString (render txs hmap (opts { primitiveMode = Triangles })) fntsDrs $ "fps:" ++ show (round (1/dt) :: Integer)
  
  
  glSwapWindow window
            
genTexObject :: Graph -> IO TextureObject
genTexObject g = do
  let --mArr = view marray g
      arr'  = view array g
      arr'' = AM.toByteString arr'
      Sz2 resx' resy' = view sz g
      --txInfo = texInfo 512 512 TexRGBA arr'
      txInfo = texInfo resx' resy' TexRGBA arr''
  t    <- loadTexture txInfo -- :: IO TextureObject
  --uuid <- nextUUID
  texture2DWrap            $= (Repeated, ClampToEdge)
  textureFilter  Texture2D $= ((Linear', Just Nearest), Linear')
  blend                    $= Enabled
  blendFunc                $= (SrcAlpha, OneMinusSrcAlpha)
  generateMipmap' Texture2D
  --return (fromMaybe nil uuid, t)
  return t

-- Graph is an Object?
initGraphResources :: Application -> [Graph] -> IO Application
initGraphResources app0 gs = do
    putStrLn "Initializing Resources..."
    -- uuid <- nextUUID
    let
      objs = introObjs ++ fntObjs ++ fgrObjs ++ bgrObjs
      txs  = concat $ concatMap (toListOf (materials . traverse . textures)) objs -- :: [Texture]
      uuids= fmap (view Texture.uuid) txs
      
      hmap'= zip uuids [0..]
      hmap = toList . fromList $ hmap'

    putStrLn "Binding Textures..."
    mapM_ (bindTexture hmap) txs
    
    mapM_ (\grph -> putStrLn $ "Generating and binding texture size : " ++ show (view sz grph)) gs
    gtxs <- mapM genTexObject gs
    let
      txs' = filter (\tx -> (head . words $ view Texture.name tx) == "Graph") txs :: [Texture]
      ids  = Prelude.read <$> concatMap (tail . words . view (Texture.name)) txs'   :: [GLuint] -- TODO: tail is unsafe, replace with Maybe
    mapM_ (uncurry bindTextureObject) (zip ids gtxs)

    putStrLn "Finished loading textures."
    return app0 { _hmap = hmap }
      where
        introObjs = concat $ toListOf (App.objects . O.foreground)  (_intro app0) :: [Object]
        fntObjs   = concat $ toListOf (App.objects . gui . O.fonts) (_main app0)  :: [Object]
        fgrObjs   = concat $ toListOf (App.objects . O.foreground)  (_main app0)  :: [Object]
        bgrObjs   = concat $ toListOf (App.objects . O.background)  (_main app0)  :: [Object]


main :: IO ()
main = do
  let argsDebug = return ["./projects/intro", "./projects/graph"]
  args <- if debug then argsDebug else getArgs

  introProj <- P.read (unsafeCoerce (args!!0) :: FilePath)
  mainProj  <- P.read (unsafeCoerce (args!!1) :: FilePath)

  let
    title = pack $ view P.name mainProj -- "Game of Life" :: String

    resx' = view P.resx mainProj
    resy' = view P.resy mainProj
    resX  = unsafeCoerce resx' :: CInt -- unsafeCoerce m :: CInt
    resY  = unsafeCoerce resy' :: CInt -- unsafeCoerce n :: CInt
    -- opts  = BackendOptions
    --         { primitiveMode = Triangles
    --         , bgrColor      = Color4 0.0 0.0 0.0 1.0
    --         , ptSize        = 3.0
    --         }
    sz'     = Sz2 resx' resy'

  window <- openWindow
            title
            (resX, resY)

  -- | SDL Mouse Options
  let camMode' =
        case view P.camMode mainProj of
          "RelativeLocation" -> RelativeLocation
          "AbsoluteLocation" -> AbsoluteLocation
          _ -> error "wrong mouse mode"

  _ <- setMouseLocationMode camMode'

  putStrLn "\n Initializing App"
  intro <- initApp initVAO introProj
  main'  <- initApp initVAO mainProj

  graph'  <- genArray resx' resy'
  mArr   <- newMArray sz' 1 :: IO (MArray RealWorld S Ix2 Word8)

  let
    gr  = Graph sz' graph' mArr
    -- grs = gr:repeat gr :: [Graph]
    
    initApp' =
      Application
      Intro -- interface current state
      intro
      main'
      []

  app' <- initGraphResources initApp' [gr]

  putStrLn "Starting App."
  animate
    window
    (parseWinInput >>> appRun app' &&& handleExit)
  return ()
