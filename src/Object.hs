{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings, Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}

module Object
  ( Object (..)
  , defaultObj
  , materials
  , programs
  , descriptors
  , transforms
  , Object.solvers
  , ObjectTree (..)
  , gui
  , foreground
  , Object.background
  , initObjectTree
  , GUI (..)
  , Object.fonts
  , icons
  , ObjectClass (..)
  , ObjectFeature (..)
-- | utility functions:  
  , initObject
  , updateObjects'
  , updateObjects
  , objectCompose
  ) where

import Control.Lens hiding (transform, pre)
import Data.Bifunctor  as BF (second)
import Data.List       as DL (transpose)
import Data.Functor              (($>))
import qualified Data.IntMap.Lazy as IM
import Data.List.Index as DL (indexed)
import FRP.Yampa    hiding (identity)
import GHC.Float
import Graphics.Rendering.OpenGL (Program (..), ShaderType (..))
import Linear.V4
import Linear.Matrix as LM -- (M44, M33, identity, translation, fromQuaternion, (!*!), mkTransformationMat)
import Linear (V3(..))

import LoadShaders
import Material
import Descriptor
import Solvable
import PGeo
import Project
import Model
import Utils          as U
--import GUI

import Debug.Trace    as DT

--------------------------------------------------------------------------------
-- < Object > ------------------------------------------------------------------

data Object
  =  Object
     {
       _descriptors :: [Descriptor] -- | Material is bound in Descriptor, but we also use this data for draw-call separation per material.
                -- data Descriptor =
                     -- Descriptor VertexArrayObject NumArrayIndices
     , _materials   :: [Material]   -- | hence [Material] is present on the Object level too, we use that value, instead of looking it up from respective VGeo.
     , _programs    :: [Program]    -- | Shader Programs
     , _transforms  :: ![M44 Double]
     , _velocity    :: V3 Double
     , _avelocity   :: V3 Double    -- | Angular velocity
     , _mass        :: Double
     , _density     :: Double
     , _time        :: Double
     , _solvers     :: [Solver]
     } deriving Show
$(makeLenses ''Object)

-- data Astronomical
--   = Astronomical
--     {
--       _descriptors :: [Descriptor] -- | Material is bound in Descriptor, but we also use this data for draw-call separation per material.
--                -- data Descriptor =
--                     -- Descriptor VertexArrayObject NumArrayIndices
--     , _materials   :: [Material]   -- | hence [Material] is present on the Object level too, we use that value, instead of looking it up from respective VGeo.
--     , _programs    :: [Program]    -- | Shader Programs
--     , _transforms  :: ![M44 Double]
--     , _velocity    :: V3 Double
--     , _avelocity   :: V3 Double    -- | Angular velocity
--     , _mass        :: Double
--     , _density     :: Double
--     , _time        :: Double
--     , _solvers     :: [Solver]
--     } deriving Show
-- $(makeLenses ''Astronomical)

-- data Sprite

data ObjectFeature
  = Transforms
  | Velocity

data ObjectClass = Foreground | Background | Font

data GUI =
     GUI
     {
       _fonts :: [Object]
     , _icons :: [Object]
     } deriving Show
$(makeLenses ''GUI)

data ObjectTree =
  ObjectTree
  {
    _gui :: GUI
  , _foreground :: [Object]
  , _background :: [Object]
  } deriving Show
$(makeLenses ''ObjectTree)

defaultObj :: Object
defaultObj =
  Object.Object
    []
    [defaultMat]
    []
    [(identity::M44 Double)]
    (V3 0 0 0)
    (V3 0 0 0)
    (1.0)
    (1.0)
    (0.0)
    []

-- | returns a list of model paths
modelPaths :: ObjectClass -> Project -> [String]
modelPaths cls project = modelList
  where
    modelSet  = toListOf (models . traverse . Model.path) project :: [String]
    modelList =
      case cls of
        Foreground -> (modelSet!!) <$> (concat $ toListOf ( objects . traverse . modelIDXs ) project)
        Background -> (modelSet!!) <$> (concat $ toListOf ( Project.background . traverse . modelIDXs ) project)
        Font       -> (toListOf (Project.fonts . traverse . Model.path) project)

initObjectTree :: (([Int], Int, [Float], Material) -> IO Descriptor) -> Project -> IO ObjectTree
initObjectTree initVAO project =
  do
    -- _ <- Dt.trace ("project :" ++ show project) $ return ()
    print "Loading Models..."

    fgrVGeos  <-
      mapM (\modelPath -> (readBGeo modelPath :: IO VGeo)
           ) $ modelPaths Foreground project  :: IO [VGeo]

    bgrVGeos  <-
      mapM (\modelPath -> (readBGeo modelPath :: IO VGeo)
           ) $ modelPaths Background project  :: IO [VGeo]

    fntVGeos  <-
      mapM (\modelPath -> (readBGeo modelPath :: IO VGeo)
           ) $ modelPaths Font project  :: IO [VGeo]

    objs  <- if not (null fgrVGeos) then mapM (initObject project initVAO Foreground) $ zip fgrVGeos [0..] else pure []  :: IO [Object]
    bgrs  <- if not (null bgrVGeos) then mapM (initObject project initVAO Background) $ zip bgrVGeos [0..] else pure []  :: IO [Object]
    fonts <- if not (null fntVGeos) then mapM (initObject project initVAO Font)       $ zip fntVGeos [0..] else pure []  :: IO [Object]

    let result =
          ObjectTree
          ( GUI fonts [] )
          objs
          bgrs

    print "Finished loading models."
    return result


initObject :: Project
           -> (([Int], Int, [Float], Material) -> IO Descriptor)
           -> ObjectClass
           -> (VGeo, Int)
           -> IO Object
initObject project
           initVAO
           cls
           (vgeo, idx) =
  do
    mats  <- mapM Material.read $ mts vgeo :: IO [Material]

    let (VGeo is_ st_ vs_ mts_ ms_ vels_ xf_) = vgeo
        vaoArgs       = (\idx' st' vao' mat' -> (idx', st', vao', mat'))
                        <$.> is_ <*.> st_ <*.> vs_ <*.> mats
        offset        = fmap (view _w . U.fromList) xf_
        vel           = toV3 (fmap float2Double (head vels_)) :: V3 Double -- TODO: this can be a list of vels, representing animated series, could be used later to create animations
        m             = realToFrac (head ms_):: Double      -- TODO: same here

        solversF      = case solvers' of
                          [] -> [""]
                          _  -> solvers'
        attrsF        = case attrs' of
                          [] -> [[]]
                          _  -> attrs'
        solvs         = toSolver <$> zip solversF attrsF
        preTransforms =
          case cls of
            Font -> U.fromList <$> xf_
            _    -> uncurry preTransformer <$> (zip solvs (fmap U.fromList xf_)::[(Solver, M44 Double)]) :: [M44 Double]
         -- _    -> (\(s0, xf0) -> preTransformer s0 xf0) <$> (zip solvs (fmap fromList xf_)::[(Solver, M44 Double)]) :: [M44 Double]

    ds    <- mapM initVAO vaoArgs

    progs <- mapM (\mat -> loadShaders
                           [ ShaderInfo VertexShader   (FileSource (_vertShader mat ))
                           , ShaderInfo FragmentShader (FileSource (_fragShader mat )) ]) mats
    let obj =
          defaultObj
          { _descriptors = ds
          , _materials   = mats
          , _programs    = progs
          , _transforms  = preTransforms
          , _velocity    = vel
          , _mass        = m
          , Object._solvers = solvs
          } :: Object

    return obj
      where
        solvers' = -- | Maybe we want very different treatment of F/B/F solvers/attrs...  or this code stinks.
          case cls of
            Foreground -> (toListOf (objects            . traverse . Project.solvers) project!!idx) :: [String]
            Background -> (toListOf (Project.background . traverse . Project.solvers) project!!idx) :: [String]
            Font       -> []
        attrs'   =
          case cls of
            Foreground -> (toListOf (objects . traverse . solverAttrs)            project!!idx) :: [[Double]]
            Background -> (toListOf (Project.background . traverse . solverAttrs) project!!idx) :: [[Double]]
            Font       -> []

fromVGeo :: (([Int], Int, [Float], Material) -> IO Descriptor) -> VGeo -> IO Object
fromVGeo initVAO (VGeo idxs st vaos matPaths mass vels xform) =
  do
    mats <- mapM Material.read matPaths -- (ms vgeo)
    let
      vaoargs         = (\idx' st' vao' mat' ->  (idx', st', vao', mat')) <$.> idxs <*.> st <*.> vaos <*.> mats
      offset = view _w (U.fromList (head xform) :: M44 Double) -- xform!!0 - at conversion stage, there can be only a single element in a list of transforms, therefore I don't want to overcomplicate it at the moment and keep it adhoc.

    ds   <- mapM initVAO vaoargs

    let object =
          defaultObj
          { _descriptors = ds
          , _materials   = mats
          }

    return object

-- | Linear Objects that only depend on initial conditions, i.e. Linear.
updateObjects :: [Object] -> SF () [Object]
updateObjects objs0 =
  proc () -> do
    objs' <- parB . fmap solve $ objs0 -< ()
    returnA -< objs'

solve :: Object -> SF () Object
solve obj0 =
  proc () -> do
    mtxs    <- (parB . fmap (transform obj0)) slvs0 -< ()
    returnA -< obj0 { _transforms = vectorizedCompose mtxs }
      where
        slvs0 = view Object.solvers obj0

transform :: Object -> Solver -> SF () ([M44 Double])
transform obj0 slv0 =
  proc () ->
    do
      mtxs <- (parB . fmap (transform' obj0 slv0)) mtxs0 -< () -- TODO: pass object as arg to transformer
      returnA -< mtxs
        where
          mtxs0 = view transforms obj0 :: [M44 Double]

transform' :: Object -> Solver -> M44 Double -> SF () (M44 Double)
transform' obj0 solver mtx0 =
  proc () -> do
    state <- case solver of
      Rotate _ _ ->
        do
          mtx' <- rotate mtx0 pv0 ypr0 -< ()
          --mtx' <- translate mtx0 ypr0 -< ()
          returnA -< mtx'
      Translate _ ->
        do
          mtx' <- translate mtx0 txyz -< ()
          returnA -< mtx'
      Gravity _ ->
        do
          returnA -< identity :: M44 Double
      _ ->
        do
          returnA -< mtx0
    returnA -< state
      where
        Rotate     pv0 ypr0 = solver
        Translate  txyz     = solver
        Gravity    idxs     = solver

-- | Objects that evolve over iterations, i.e. non-Linear
updateObjects' :: [Object] -> SF [Object] [Object]
updateObjects' objs0 =
  proc objs -> do
    rec objs   <- iPre objs0 -< objs'
        objs'  <- gravitySolver -< objs
    returnA -< objs

gravitySolver :: SF [Object] [Object]
gravitySolver =
  proc objs0 -> do
    -- let objsG = filter (\x -> any (\case Gravity {} -> True; _ -> False) (view Object.solvers x)) objs'
    let objs = (gravitySolver' <$> decomp objs0) :: [Object]
    returnA -< objs

gravitySolver' :: (Object, [Object]) -> Object
gravitySolver' (obj0, objs0) = obj
  where
    m0     =  _mass obj0               :: Double
    xform0 = (head . _transforms) obj0 :: M44 Double
    p0     = ( view (_w._xyz) . LM.transpose ) xform0  :: V3 Double

    ms     = fmap _mass objs0          :: [Double]
    xforms = fmap (head . _transforms) objs0              :: [M44 Double]
    ps     = fmap ( view (_w._xyz) . LM.transpose) xforms :: [V3 Double]

    acc = sum $ fmap (gravity p0 m0) (zip ps ms) :: V3 Double
    -- acc = sum $ fmap (gravity (DT.trace ("p0 :" ++ show p0) p0)
    --                           (DT.trace ("m0 :" ++ show m0 ++ "\n") m0)) (zip (DT.trace ("\n\n ps :" ++ show ps) ps)
    --                                                                   (DT.trace ("ms :" ++ show ms) ms)) :: V3 Double

    s    = 100000000.0*1.0
    s1   = 0.0

    vel = (_velocity obj0)*s1 + (acc*s)     :: V3 Double
    mtx =
      mkTransformationMat
      rot
      tr
      where
        rot = view _m33 xform0
        --tr  = (DT.trace ("acc :" ++ show (s*acc) ++ "\n\n") $ s*acc) -- + p0*0.01
        tr  = vel + p0
        --tr  = (V3 1 0 0) + p0
        --tr  = acc*s

    obj = obj0 { _transforms = [mtx]
               , _velocity   = vel }

g = 6.673**(-11.0) :: Double
--g = 6.673 :: Double

gravity :: V3 Double -> Double -> (V3 Double, Double) -> V3 Double
gravity p0 m0 (p1, m1) = acc --(DT.trace ("gravity.acc :" ++ show (norm (1000000000000.0*acc)) ++ "\n") acc)
  where
    dir  = p1 ^-^ p0                 :: V3 Double
    dist = norm dir                  :: Double
    --dist = (DT.trace ("dist :" ++ show (norm dir) ++ "\n") norm dir) :: Double
    f    = g * m0 * m1 / dist**2.0   :: Double
    --s    = 10000000.0*0.0
    acc  = (f / m1) *^ (dir ^/ dist) :: V3 Double
    --acc  = ((DT.trace("f :" ++ show f)f) / m1) *^ ((DT.trace ("dir :" ++ show dir) dir) ^/ (DT.trace ("dist :" ++ show dist) dist)) :: V3 Double
-- | F = G*@mass*m2/(dist^2);       // Newton's gravity equation
-- | a += (F/@mass)*normalize(dir); // Acceleration

decomp :: [a] -> [(a, [a])]
decomp = fmap decomp' . rotatedLists

decomp' :: [a] -> (a, [a])
decomp' xs = (head (take 1 xs), drop 1 xs)

rotatedLists :: [a] -> [[a]]
rotatedLists xs = rotateList' <$> DL.indexed (replicate (length xs) xs)

vectorizedCompose :: [[M44 Double]] -> [M44 Double]
vectorizedCompose = fmap (foldr1 (^*^)) . DL.transpose

(^*^) :: M44 Double -> M44 Double -> M44 Double
(^*^) mtx0 mtx1 = mkTransformationMat rot tr
  where
    rot = view _m33 mtx0 !*! view _m33 mtx1 :: M33 Double
    tr  = view translation mtx0 ^+^ view translation mtx1

objectCompose :: ObjectFeature -> (Double, Double) -> [Object] -> [Object] -> [Object]
objectCompose f ratios objs0 objs1 = undefined
