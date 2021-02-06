{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Arrows #-}

module Solvable
  ( Solver (..)
  , preTransformer
--  , transformer
  , translate
  , rotate
--  , gravity
  , toSolver
  ) where

import Control.Lens      hiding (Identity)
import GHC.Float
import Linear.Matrix     hiding (identity)
import Linear.V3
import Linear.V4
import Linear.Quaternion hiding (rotate)
import Control.Lens      hiding (transform, Identity)
import FRP.Yampa         hiding (identity)

import Utils (toV3)

import Debug.Trace as DT

data Solver =
     Identity
  |  PreTranslate
     {
       _txyz   :: V3 Double
     }
  |  Translate
     {
       _txyz   :: V3 Double
     }
  |  PreRotate
     {
       _pivot :: V3 Double
     , _ypr   :: V3 Double
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
    --   _vel  :: V3 Double
    -- , _m    :: Double
    --   _ps   :: [V3 Double]
    -- , _ms   :: [Double]
      _idxs :: [Int]
    } deriving Show
$(makeLenses ''Solver)

toSolver :: (String, [Double]) -> Solver
toSolver (solver, parms) =
  --case (DT.trace ("toSolver.solver :" ++ show solver) $ solver) of
  case solver of
    "pretranslate" -> PreTranslate (toV3 parms)
    "translate"    -> Translate    (toV3 parms)
    "prerotate"    -> PreRotate    (toV3 $ take 3 parms) (toV3 $ drop 3 parms)
    "rotate"       -> Rotate       (toV3 $ take 3 parms) (toV3 $ drop 3 parms)
    "gravity"      -> Gravity      (double2Int <$> parms)
    _              -> Identity
  -- where
  --   idxs' = undefined :: [Int]

preTransformer :: Solver -> M44 Double -> M44 Double
preTransformer solver mtx0 = mtx
  where
    mtx = case solver of
      PreTranslate v0    -> preTranslate mtx0 v0
      PreRotate pv0 ypr0 -> preRotate mtx0 pv0 ypr0
      Identity           -> identity mtx0
      _                  -> mtx0

identity :: M44 Double -> M44 Double
identity mtx0 = mtx
  where
    mtx =
      mkTransformationMat
        rot
        tr
        where
          rot = view _m33 mtx0
          tr  = view (_w._xyz) mtx0

preTranslate :: M44 Double -> V3 Double -> M44 Double
preTranslate mtx0 v0 = mtx
  where
    mtx =
      mkTransformationMat
        rot
        tr
        where
          rot = view _m33 mtx0
          tr  = v0 + view (_w._xyz) mtx0

preRotate :: M44 Double -> V3 Double -> V3 Double -> M44 Double
preRotate mtx0 pv0 ypr0 = mtx
    where
      mtx =
          mkTransformationMat
            rot
            tr
            where
              rot =
                view _m33 mtx0
                !*! fromQuaternion (axisAngle (view _x (view _m33 mtx0)) (view _x ypr0)) -- yaw
                !*! fromQuaternion (axisAngle (view _y (view _m33 mtx0)) (view _y ypr0)) -- pitch
                !*! fromQuaternion (axisAngle (view _z (view _m33 mtx0)) (view _z ypr0)) -- roll
              tr  = view (_w._xyz) mtx0

translate :: M44 Double -> V3 Double -> SF () (M44 Double)
translate mtx0 v0 =
  proc () -> do
    tr' <- (V3 0 0 0 ^+^) ^<< integral -< v0
    let mtx =
          mkTransformationMat
            (view _m33 mtx0)
            tr'

    returnA -< mtx

rotate :: M44 Double -> V3 Double -> V3 Double -> SF () (M44 Double)
rotate mtx0 pv0 ypr0 =
  proc () -> do
    ypr' <- (V3 0 0 0 ^+^) ^<< integral -< ypr0
    let mtx =
          mkTransformationMat
            rot
            tr
            where
              rot =
                view _m33 mtx0
                !*! fromQuaternion (axisAngle (view _x (view _m33 mtx0)) (view _x ypr')) -- yaw
                !*! fromQuaternion (axisAngle (view _y (view _m33 mtx0)) (view _y ypr')) -- pitch
                !*! fromQuaternion (axisAngle (view _z (view _m33 mtx0)) (view _z ypr')) -- roll
              tr  = (view (_w._xyz) . transpose) mtx0 
    returnA -< mtx

g = 6.673**(-11.0) :: Double

-- gravity :: M44 Double -> V3 Double -> Double -> [V3 Double] -> [Double] -> SF () (M44 Double)
-- gravity mtx0 v0 m0 ps ms =
--   proc () -> do
--     let
--       p0 = view (_w._xyz) mtx0
--     --acc <- ((V3 0 0 0) ^+^) ^<< integral -< a0

--       a0 = foldr1 (^+^) $ (100000000000000.0 *^) . gravity' p0 m0 <$> zip ps ms :: V3 Double
--       --a0 = foldr1 (^+^) $ fmap ((100000000000000.0 *^) . gravity' p0 m0) $ zip (DT.trace ("gravity.ps :" ++ show ps) ps) ms :: V3 Double
--     --acc  <- (v0 ^+^) ^<< integral -< a0
--     acc  <- (v0 ^+^) ^<< integral -< a0

--     -- mtx <- translate mtx0 acc -< ()
--     let mtx =
--           mkTransformationMat
--             rot
--             tr
--             where
--               rot =
--                 (view _m33 mtx0)
--               tr = acc
--     returnA -< mtx
--     --returnA -< (DT.trace ("gravity.mtx :" ++ show mtx) $ mtx)
--     --returnA -< (DT.trace ("gravity.ps :" ++ show ps) $ mtx)

-- gravity'' :: V3 Double -> Double -> (V3 Double, Double) -> SF () (V3 Double)
-- gravity'' p0 m0 (p1, m1) =
--   proc () -> do
--     let
--       dir  = p1 ^-^ p0                                    :: V3 Double
--       dist = norm (DT.trace ("dir :" ++ show dir) $ dir)  :: Double
--       f    = g * m0 * m1 / dist**2.0                      :: Double
--       acc  = (f / m1) *^ (dir ^/ dist)                    :: V3 Double
--     returnA -< acc

gravity' :: V3 Double -> Double -> (V3 Double, Double) -> V3 Double
gravity' p0 m0 (p1, m1) = acc
  where
    dir  = p1 ^-^ p0                 :: V3 Double
    dist = norm dir                  :: Double
    --dist = norm (DT.trace ("dir :" ++ show dir) $ dir)                  :: Double
    f    = g * m0 * m1 / dist**2.0   :: Double
    acc  = (f / m1) *^ (dir ^/ dist) :: V3 Double
-- | F = G*@mass*m2/(dist^2);       //  Newton's gravity equation
-- | a += (F/@mass)*normalize(dir); // Acceleration
