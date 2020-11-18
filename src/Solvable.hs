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
    , _mass :: Double
    }
  deriving Show
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
          mtx' <- spin pv0 ypr0 mtx0 -< ()
          --mtx' <- spin pv0 ypr0 (DT.trace ("transformer mtx0 :" ++ show mtx0) $ mtx0) -< ()
          returnA -< mtx'
      Translate _ ->
        do
          mtx' <- translate txyz mtx0 -< ()
          returnA -< mtx'
    returnA -< state
    --returnA -< (DT.trace ("transformer state :" ++ show state)$ state)
      where
        Rotate pv0 ypr0 = solver
        Translate txyz  = solver

-- TODO: write a "translate solver

spin :: V3 Double -> V3 Double -> M44 Double -> SF () (M44 Double)
spin pv0 ypr0 mtx0 =
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

translate :: V3 Double -> M44 Double -> SF () (M44 Double)
translate v0 mtx0 =
  proc () -> do
    tr' <- ((V3 0 0 0) ^+^) ^<< integral -< v0
    let mtx =
          mkTransformationMat
            rot
            tr
            where
              rot =
                (view _m33 mtx0)
              tr = v0
    returnA -< mtx

g = 6.673**(-11.0) :: Double

gravity :: V3 Double -> Double -> [V3 Double] -> [Double] -> SF () (M44 Double)
gravity p0 m0 vs ms =
  proc () -> do
    let a0 = foldr1 (^+^) $ fmap (gravity' p0 m0) $ zip vs ms :: V3 Double
    acc <- ((V3 0 0 0) ^+^) ^<< integral -< a0
    -- let mtx =
    --       mkTransformationMat
    --         rot
    --         tr
    --         where
    --           rot =
    --             (view _m33 mtx0)
    --           tr = p0
          
    returnA -< undefined
  
gravity' :: V3 Double -> Double -> (V3 Double, Double) -> V3 Double
gravity' p0 m0 (p1, m1) = acc
  where
    dir  = p1 ^-^ p0                 :: V3 Double
    dist = norm dir                  :: Double
    f    = g * m0 * m1 / dist**2.0   :: Double
    acc  = (f / m1) *^ (dir ^/ dist) :: V3 Double
-- | F = G*@mass*m2/(dist^2);       //  Newton's gravity equation
-- | a += (F/@mass)*normalize(dir); // Acceleration
