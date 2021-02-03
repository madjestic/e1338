{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings, Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Object
  ( Object (..)
  , defaultObj
  , materials
  , programs
  , descriptors
  , transforms
  , solveLinear
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
-- | utility functions:  
  , initObject
  , updateObjects
  , updateObjects'
  , updateObjects''
  , updateObjects2
  ) where

import GHC.Float
import Linear.V4
import Linear.Matrix as LM -- (M44, M33, identity, translation, fromQuaternion, (!*!), mkTransformationMat)
import Linear (V3(..))
import Control.Lens hiding (transformLinear)
import FRP.Yampa    hiding (identity)
import Graphics.Rendering.OpenGL (Program (..), ShaderType (..))
import Data.List as DL (transpose)
import Data.Functor              (($>))
--import Data.VectorSpace
import Data.List.Index as DL (indexed)

import LoadShaders
import Material
import Descriptor
import Solvable
import PGeo
import Project
import Model
import Utils
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

-- TODO: This is wrong, as it will return duplicate models, it should be a list of Objects with indexes matching the VGeos instead...
-- | returns a list of model paths
modelPaths :: ObjectClass -> Project -> [String]
modelPaths cls project = modelList
  where
    modelSet  = toListOf (models . traverse . path) project :: [String]
    modelList =      
      case cls of
        Foreground -> (modelSet!!) <$> (concat $ toListOf ( objects . traverse . modelIDXs ) project)
        Background -> (modelSet!!) <$> (concat $ toListOf ( Project.background . traverse . modelIDXs ) project)
        Font       -> (toListOf (Project.fonts . traverse . path) project)

initObjectTree :: (([Int], Int, [Float], Material) -> IO Descriptor) -> Project -> IO ObjectTree
initObjectTree initVAO project = 
  do
    -- _ <- Dt.trace ("project :" ++ show project) $ return ()
    print "Loading Models..."

    -- pgeo <- readPGeo "./models/body_0.pgeo"
    -- print $ "initObjectTree.pgeo" ++ show pgeo

    -- vgeo <- readBGeo "./models/body_0.bgeo"
    -- print $ "initObjectTree.vgeo" ++ show vgeo

    fgrVGeos  <-
      mapM (\modelPath ->
               do { vgeo <- readBGeo modelPath :: IO VGeo --TODO: error here, seems to be empty
                  ; return vgeo
                  }
           ) $ (modelPaths Foreground project ) :: IO [VGeo]
    -- print ("initObjectTree.fgrVGeos :" ++ show fgrVGeos)

    bgrVGeos  <-
      mapM (\modelPath ->
               do { vgeo <- readBGeo modelPath :: IO VGeo
                  ; return vgeo
                  }
           ) $ (modelPaths Background project ) :: IO [VGeo]
                                                                                                                            
    fntVGeos  <-
      mapM (\modelPath ->
               do { vgeo <- readBGeo modelPath :: IO VGeo
                  ; return vgeo
                  }
           ) $ (modelPaths Font project ) :: IO [VGeo]
                                              
    objs  <- if (length fgrVGeos > 0 ) then (mapM (initObject project initVAO Foreground) $ zip fgrVGeos [0..]) else pure []  :: IO [Object]
    bgrs  <- if (length bgrVGeos > 0 ) then (mapM (initObject project initVAO Background) $ zip bgrVGeos [0..]) else pure []  :: IO [Object]
    fonts <- if (length fntVGeos > 0 ) then (mapM (initObject project initVAO Font)       $ zip fntVGeos [0..]) else pure []  :: IO [Object]

    let result =
          ObjectTree
          ( GUI fonts [] )
          objs
          bgrs

    print "Finished loading models."
    return (result)


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
    print "Loading Materials..."
    mats  <- mapM readMaterial $ mts vgeo :: IO [Material]
    
    let (VGeo is_ st_ vs_ mts_ ms_ vels_ xf_) = vgeo
        vaoArgs       = (\idx' st' vao' mat' -> (idx', st', vao', mat'))
                        <$.> is_ <*.> st_ <*.> vs_ <*.> mats
        offset        = fmap ((view _w).fromList) (xf_)
        vel           = toV3 (fmap float2Double (vels_!!0)) :: V3 Double -- TODO: this can be a list of vels, representing animated series, could be used later to create animations
        m             = realToFrac (ms_!!0):: Double      -- TODO: same here

        solversF      = case solvers' of
                          [] -> [""]
                          _  -> solvers'
        attrsF        = case attrs' of
                          [] -> [[]]
                          _  -> attrs'
        solvs         = fmap toSolver $ zip solversF attrsF
        preTransforms =
          case cls of
            Font -> fmap fromList ((xf_))
            _    -> fmap (\(s0, xf0) -> preTransformer s0 xf0) $ (zip solvs (fmap fromList ((xf_)))::[(Solver, M44 Double)]) :: [M44 Double]
        
    ds    <- mapM initVAO vaoArgs

    progs <- mapM (\mat -> loadShaders
                           [ ShaderInfo VertexShader   (FileSource (_vertShader (mat) ))
                           , ShaderInfo FragmentShader (FileSource (_fragShader (mat) )) ]) mats
    let obj =
          defaultObj
          { _descriptors = ds
          , _materials   = mats
          , _programs    = progs
          , _transforms  = preTransforms
          , _velocity    = (DT.trace ("InitObject.vel :" ++ show vel ) $ vel)
          , _mass        = (DT.trace ("InitObject.m   :" ++ show m ) $ m)
          , Object._solvers = (DT.trace ("InitObject.solvs :" ++ show solvs) $ solvs)
          } :: Object

    return obj
      where
        solvers' =
          case cls of
            Foreground -> ((toListOf (objects            . traverse . Project.solvers) project)!!idx) :: [String] 
            Background -> ((toListOf (Project.background . traverse . Project.solvers) project)!!idx) :: [String]
            Font       -> []            
        attrs'   =
          case cls of
            Foreground -> ((toListOf (objects . traverse . solverAttrs)            project)!!idx) :: [[Double]]
            Background -> ((toListOf (Project.background . traverse . solverAttrs) project)!!idx) :: [[Double]]
            Font       -> []

fromVGeo :: (([Int], Int, [Float], Material) -> IO Descriptor) -> VGeo -> IO Object
fromVGeo initVAO (VGeo idxs st vaos matPaths mass vels xform) = 
  do
    mats <- mapM readMaterial matPaths -- (ms vgeo)
    let
      vaoargs         = (\idx' st' vao' mat' ->  (idx', st', vao', mat')) <$.> idxs <*.> st <*.> vaos <*.> mats
      offset = view _w (fromList (xform!!0) :: M44 Double) -- xform!!0 - at conversion stage, there can be only a single element in a list of transforms, therefore I don't want to overcomplicate it at the moment and keep it adhoc.

    ds   <- mapM initVAO vaoargs
    
    let object =
          defaultObj
          { _descriptors = ds
          , _materials   = mats
          }

    return object
    
solveLinear :: [Object] -> Object -> SF () Object
solveLinear objs obj =
  proc () -> do
    mtxs <- (parB . fmap (transformLinear objs obj)) slvs0 -< ()
    --mtxs <- (parB . fmap (transformLinear (DT.trace ("solveLinear.objs :" ++ show objs) $ objs) obj)) slvs0 -< ()
    returnA -< obj { _transforms = vectorizedCompose mtxs }
      where
        slvs0 = view Object.solvers obj
        -- mtxs0 = view transforms     obj

solveLinear' :: Object -> SF [Object] Object
solveLinear' obj0 =
  proc objs -> do

    mtxs <- (parB . fmap (transformLinear' obj0)) slvs0 -< objs
    returnA -< obj0 { _transforms = vectorizedCompose mtxs }
      where
        slvs0 = view Object.solvers obj0

-- solveLinear' :: SF [Object] [Object]
-- solveLinear' =
--   proc objs0 -> do
--     objs <- transformLinear' -< objs0
--     returnA -< objs

transformLinear' :: Object -> Solver -> SF [Object] ([M44 Double])
transformLinear' obj0 slv0 =
  proc objs ->
    do
      mtxs <- (parB . fmap (transformerLinear' obj0 slv0)) mtxs0 -< objs -- TODO: pass object as arg to trabsformer
      --mtxs <- (parB . fmap (transformerLinear (DT.trace ("transformLinear.objs :" ++ show objs) $ objs) obj0 slv0)) mtxs0 -< () -- TODO: pass object as arg to trabsformer
      returnA -< mtxs
        where
          mtxs0 = view transforms obj0 :: [M44 Double]


transformLinear :: [Object] -> Object -> Solver -> SF () ([M44 Double])
transformLinear objs obj0 slv0 = 
  proc () ->
    do
      mtxs <- (parB . fmap (transformerLinear objs obj0 slv0)) mtxs0 -< () -- TODO: pass object as arg to trabsformer
      --mtxs <- (parB . fmap (transformerLinear (DT.trace ("transformLinear.objs :" ++ show objs) $ objs) obj0 slv0)) mtxs0 -< () -- TODO: pass object as arg to trabsformer
      returnA -< mtxs
        where
          mtxs0 = view transforms obj0 :: [M44 Double]

transformerLinear :: [Object] -> Object -> Solver -> M44 Double -> SF () (M44 Double)
transformerLinear objs obj0 solver mtx0 =
  proc () -> do
    state <- case solver of
      Rotate _ _ ->
        do
          mtx' <- rotate mtx0 pv0 ypr0 -< ()
          --mtx' <- rotate (DT.trace ("transformerLinear.Rotate.objs :" ++ show objs) $ mtx0) pv0 ypr0 -< ()
          returnA -< mtx'
      Translate _ ->
        do
          mtx' <- translate mtx0 txyz -< ()
          returnA -< mtx'
      -- Gravity _ ->
      --   do
      --     mtx' <- gravityLinear (objs, obj0) -< ()
      --     returnA -< mtx'
      _ ->
        do
          returnA -< mtx0
    returnA -< state
      where
        v0 = view velocity obj0
        m0 = view mass obj0
        ps = fmap (view (_w._xyz) . head . (view transforms)) ([objs!!0]):: [V3 Double]
        ms = fmap (view mass) objs :: [Double]
        Rotate     pv0 ypr0 = solver
        Translate  txyz     = solver
        Gravity    idxs     = solver

transformerLinear' :: Object -> Solver -> M44 Double -> SF [Object] (M44 Double)
transformerLinear' obj0 solver mtx0 =
  proc objs -> do
    state <- case solver of
      -- Rotate _ _ ->
      --   do
      --     mtx' <- rotate mtx0 pv0 ypr0 -< ()
      --     --mtx' <- rotate (DT.trace ("transformerLinear.Rotate.objs :" ++ show objs) $ mtx0) pv0 ypr0 -< ()
      --     returnA -< mtx'
      Translate _ ->
        do
          mtx' <- translate mtx0 txyz -< ()
          returnA -< mtx'
      -- Gravity _ ->
      --   do
      --     mtx' <- gravityLinear (objs, obj0) -< ()
      --     returnA -< mtx'
      _ ->
        do
          returnA -< mtx0
    returnA -< state
      where
    --     v0 = view velocity obj0
    --     m0 = view mass obj0
    --     ps = fmap (view (_w._xyz) . head . (view transforms)) ([objs!!0]):: [V3 Double]
    --     ms = fmap (view mass) objs :: [Double]
    --     Rotate     pv0 ypr0 = solver
        Translate  txyz     = solver
    --     Gravity    idxs     = solver

gravityLinear :: ([Object], Object) -> SF () (M44 Double)
gravityLinear (objs, obj0) =
  proc () -> do
    let
      mtxs0 = view transforms obj0
      
      mtx0 = mtxs0!!0
      objs' = take 2 objs

      v0 = view velocity obj0
      m0 = view mass obj0
      ps = fmap (view (_w._xyz) . head . (view transforms)) objs' :: [V3 Double]
      ms = fmap (view mass) objs' :: [Double]
      
      p0 = view (_w._xyz) mtx0
      --a0 = foldr1 (^+^) $ fmap ((100000000000000.0 *^) . gravityLinear' p0 m0) $ zip (DT.trace ("gravity.ps :" ++ show ps) $ ps) ms :: V3 Double
      a0 = foldr1 (^+^) $ fmap ((1000000000000000.0 *^) . gravityLinear' p0 m0) $ zip ps ms :: V3 Double

    acc  <- ((view velocity obj0) ^+^) ^<< integral -< a0

    let mtx =
          mkTransformationMat
            rot
            tr
            where
              rot =
                (view _m33 mtx0)
              tr = acc
    returnA -< mtx

g = 6.673**(-11.0) :: Double

gravityLinear' :: V3 Double -> Double -> (V3 Double, Double) -> V3 Double
gravityLinear' p0 m0 (p1, m1) = acc
  where
    dir  = p1 ^-^ p0                 :: V3 Double
    --dir  = (DT.trace ("gravityLinear'.p1 :" ++ show p1) $ p1) ^-^ (DT.trace ("gravityLinear'.p0 :" ++ show p0)$ p0) :: V3 Double
    dist = norm dir                  :: Double
    --dist = norm (DT.trace ("gravityLinear'.dir :" ++ show dir) $ dir)                        :: Double
    f    = g * m0 * m1 / dist**2.0   :: Double
    acc  = (f / m1) *^ (dir ^/ dist) :: V3 Double
-- | F = G*@mass*m2/(dist^2);       // Newton's gravity equation
-- | a += (F/@mass)*normalize(dir); // Acceleration

gravitySolver :: SF [Object] [Object]
gravitySolver =
  proc objs0 -> do
    let objs = fmap gravitySolver' $ decomp objs0 :: [Object]
    returnA -< objs

decomp :: [a] -> [(a, [a])]
decomp = fmap decomp' . rotatedLists

decomp' :: [a] -> (a, [a])
decomp' xs = (head (take 1 xs), drop 1 xs)

rotatedLists :: [a] -> [[a]]
rotatedLists xs = fmap rotateList' $ DL.indexed $ take (length xs) $ repeat xs

gravitySolver' :: (Object, [Object]) -> Object
gravitySolver' (obj0, objs0) = obj
  where
    m0     =  _mass obj0               :: Double
    xform0 = (head . _transforms) obj0 :: M44 Double
    p0     = ( view (_w._xyz) . LM.transpose ) xform0  :: V3 Double
    
    ms     = foldr1 (+) $ fmap (_mass) objs0              :: Double
    xforms = fmap (head . _transforms) objs0              :: [M44 Double]
    ps     = foldr1 (^+^) $ fmap ( view (_w._xyz) . LM.transpose) (xforms) :: V3 Double
    --ps     = foldr1 (^+^) $ fmap ( view (_w._xyz)) (DT.trace ("xforms" ++ show xforms) $ xforms) :: V3 Double

    dir  = ps ^-^ p0                 :: V3 Double
    --dir  = (DT.trace ("ps" ++ show ps) $ ps) ^-^ (DT.trace ("p0" ++ show p0) $ p0) :: V3 Double
    dist = norm dir                  :: Double
    --dist = 0.5
    --dist = 1.0 + (norm dir)                  :: Double
    f    = g * m0 * ms / dist**2.0   :: Double
    acc  = (f / ms) *^ (dir ^/ dist) :: V3 Double
    --acc  = ((DT.trace ("f" ++ show f) $ f) / (DT.trace ("ms" ++ show ms) $ ms)) *^ ((DT.trace ("dir" ++ show dir) $ dir) ^/ (DT.trace ("dist" ++ show dist) $ dist)) :: V3 Double
    s    = 100000000000.0
    
    vel = (_velocity obj0) + acc*s     :: V3 Double
    --vel = (_velocity obj0) + (DT.trace ("acc:" ++ show acc) $ acc)     :: V3 Double
    mtx =  
      mkTransformationMat
      rot
      tr
      where
        rot = (view _m33 xform0)
        --tr  = (vel) + p0
        tr  = s*(DT.trace ("acc :" ++ show acc) $ acc) -- + p0*0.01
        --tr  = p0

    obj = obj0 { _transforms = [mtx]
               , _velocity   = vel }

vectorizedCompose :: [[M44 Double]] -> [M44 Double]
vectorizedCompose = fmap (foldr1 (^*^)) . DL.transpose

(^*^) :: M44 Double -> M44 Double -> M44 Double
(^*^) mtx0 mtx1 = mkTransformationMat rot tr
  where
    rot = (view _m33 mtx0) !*! (view _m33 mtx1) :: M33 Double
    tr  = (view translation mtx0) ^+^ (view translation mtx1)

updateObjects :: [Object] -> SF () [Object]
updateObjects objs0 =
  proc () -> do
    objs <- parB . fmap (solveLinear objs0) $ objs0 -< ()
    --objs <- parB . fmap (solveLinear objs0) $ (DT.trace ("updateObjects.objs0 " ++ show objs0 ) $ objs0) -< ()
    returnA -< objs
    --returnA -< (DT.trace ("updateObjects.objs :" ++ show objs ) $ objs)

updateObjects2 :: [Object] -> SF [Object] [Object]
updateObjects2 objs0 =
  proc objs -> do
    objs' <- parB . fmap solveLinear' $ objs0 -< objs
    objs'' <- gravitySolver -< objs'
    returnA -< objs''

updateObjects' :: SF [Object] [Object] -- 013121, latest iteration
updateObjects' =
  proc objs -> do
    --rec objs  <- iPre objs -< objs'
    objs'   <- gravitySolver -< objs
    --objs' <- parB (fmap gravitySolver objs0) -< objs
    returnA -< objs'

updateObjects'' :: [Object] -> SF [Object] [Object] -- 013121, latest iteration
updateObjects'' objs0 =
  proc objs -> do
    --rec objs  <- iPre objs0 -< objs'
    rec
      objs  <- iPre objs0 -< objs'
      --objs' <- gravitySolver -< objs
      objs'  <- updateObjects objs0 -< ()
    --objs' <- parB (fmap gravitySolver objs0) -< objs
    returnA -< objs'
