{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Arrows #-}

module Solvable
  ( Solver (..)
--  , Solvable (..)
  , transformer
  , spin
  , fromString
  ) where

import Control.Lens
import Linear.Matrix
import Linear.V3
import Linear.V4
import Linear.Quaternion
import Control.Lens hiding (transform)
import FRP.Yampa

import Utils (toV3)

import Debug.Trace as DT

data Solver =
     Translate
     {
       _txyz   :: V3 Double
     }
  |  Rotate
     {
       _pivot :: V3 Double
     , _ypr   :: V3 Double
     }
  |  Scale
     {
       _sxyz   :: V3 Double
     }
  |  LOD
     {
       _models :: [FilePath]
     }
  |  Gravity
    {
      _vel  :: V3 Double
    , _m    :: Double
    , _ps   :: [V3 Double]
    , _ms   :: [Double]
    } deriving Show
$(makeLenses ''Solver)

fromString :: (String, [Int]) -> Solver
fromString (x, ys) =
  case x of
    "spin" -> Rotate (toV3 $ take 3 (fmap toEnum ys :: [Double])) (toV3 $ drop 3 (fmap toEnum ys :: [Double]))
    _ -> undefined

transformer :: Solver -> M44 Double -> SF () (M44 Double)
transformer solver mtx0 =
  proc () -> do
    --state <- case (DT.trace ("solver :" ++ show solver) $ solver) of
    state <- case solver of
      Rotate _ _ ->
        do
          mtx' <- spin mtx0 pv0 ypr0 -< ()
          --mtx' <- spin pv0 ypr0 (DT.trace ("transformer mtx0 :" ++ show mtx0) $ mtx0) -< ()
          returnA -< mtx'
      Translate _ ->
        do
          mtx' <- translate mtx0 txyz -< ()
          returnA -< mtx'
      Gravity _ _ _ _ ->
        do
          mtx' <- gravity mtx0 v0 m0 ps ms -< ()
          returnA -< mtx'
    returnA -< state
    --returnA -< (DT.trace ("transformer state :" ++ show state)$ state)
      where
        Rotate pv0 ypr0 = solver
        Translate  txyz = solver
        Gravity v0 m0 ps ms = solver

-- TODO: write a "translate solver

spin :: M44 Double -> V3 Double -> V3 Double -> SF () (M44 Double)
spin mtx0 pv0 ypr0 =
  proc () -> do
    ypr' <- ((V3 0 0 0) ^+^) ^<< integral -< ypr0
    let mtx =
          mkTransformationMat
            rot
            tr
            where
              rot =
                (view _m33 mtx0)
                !*! fromQuaternion (axisAngle (view _x (view _m33 mtx0)) (view _x ypr')) -- yaw
                !*! fromQuaternion (axisAngle (view _y (view _m33 mtx0)) (view _y ypr')) -- pitch
                !*! fromQuaternion (axisAngle (view _z (view _m33 mtx0)) (view _z ypr')) -- roll
              tr  = view (_w._xyz) mtx0

    returnA -< mtx

translate :: M44 Double -> V3 Double -> SF () (M44 Double)
translate mtx0 v0 =
  proc () -> do
    tr' <- ((V3 0 0 0) ^+^) ^<< integral -< v0
    let mtx =
          mkTransformationMat
            rot
            tr
            where
              rot =
                (view _m33 mtx0)
              tr = tr'
    returnA -< mtx

g = 6.673**(-11.0) :: Double

gravity :: M44 Double -> V3 Double -> Double -> [V3 Double] -> [Double] -> SF () (M44 Double)
gravity mtx0 v0 m0 ps ms =
  proc () -> do
    let
      p0 = view (_w._xyz) mtx0
      a0 = foldr1 (^+^) $ fmap (gravity' p0 m0) $ zip ps ms :: V3 Double
    --acc <- ((V3 0 0 0) ^+^) ^<< integral -< a0
    acc  <- (v0 ^+^) ^<< integral -< a0
    -- mtx <- translate mtx0 acc -< ()
    let mtx =
          mkTransformationMat
            rot
            tr
            where
              rot =
                (view _m33 mtx0)
              tr = acc
    returnA -< mtx
  
gravity' :: V3 Double -> Double -> (V3 Double, Double) -> V3 Double
gravity' p0 m0 (p1, m1) = acc
  where
    dir  = p1 ^-^ p0                 :: V3 Double
    dist = norm dir                  :: Double
    f    = g * m0 * m1 / dist**2.0   :: Double
    acc  = (f / m1) *^ (dir ^/ dist) :: V3 Double
-- | F = G*@mass*m2/(dist^2);       //  Newton's gravity equation
-- | a += (F/@mass)*normalize(dir); // Acceleration
