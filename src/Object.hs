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
  , solve
  , Object.solvers
  , ObjectTree (..)
  , gui
  , foreground
  , Object.background
  , loadObjects
  , GUI (..)
  , Object.fonts
  , icons
  , ObjectClass (..)
-- | utility functions:  
  , initObject
  , updateObjects
  ) where

import Linear.V4
import Linear.Matrix -- (M44, M33, identity, translation, fromQuaternion, (!*!), mkTransformationMat)
import Linear (V3(..))
import Control.Lens hiding (transform)
import FRP.Yampa    hiding (identity)
import Graphics.Rendering.OpenGL (Program (..), ShaderType (..))
import Data.List as DL (transpose)

import LoadShaders
import Material
import Descriptor
import Solvable
import PGeo
import VGeo
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
     , _transforms  :: [M44 Double]
     , _velocity    :: V3 Double
     , _avelocity   :: V3 Double    -- | Angular velocity
     , _mass        :: Double
     , _density     :: Double
     , _time        :: Double
     , _solvers     :: [Solver]
     } deriving Show

$(makeLenses ''Object)

data GUI =
     GUI
     {
       _fonts :: [Object]
     , _icons :: [Object]
     } deriving Show

$(makeLenses ''GUI)

-- defaultGUI :: GUI
-- defaultGUI = GUI [] []

data ObjectTree =
  ObjectTree
  {
    _gui :: GUI
  , _foreground :: [Object]
  , _background :: [Object]
  } deriving Show

data ObjectClass = Foreground | Background | Font

$(makeLenses ''ObjectTree)

-- -- TODO : take translation (pivot offset) into account
-- instance Solvable Object where
--   solver :: Solver -> Object -> SF () (Object)
--   solver (Rotate pv0 ypr0) obj0 = -- TODO: there are multiple transforms per object
--     proc () -> do
--       --ypr' <- ((V3 0 0 0) ^+^) ^<< integral -< ypr0
--       ypr' <- ((V3 0 0 0) ^+^) ^<< integral -< ypr0
--       -- _ <- DT.trace ("Object._transforms obj0 :" ++ show (Object._transforms obj0)) $ returnA -< ()
--       let mtx0 = Object._transforms obj0
--           mtx = undefined :: [M44 Double]
--             -- mkTransformationMat
--             -- rot
--             -- tr
--             -- where
--             --   rot =
--             --     (view _m33 mtx0)
--             --     !*! fromQuaternion (axisAngle (view _x (view _m33 mtx0)) (view _x ypr')) -- yaw
--             --     !*! fromQuaternion (axisAngle (view _y (view _m33 mtx0)) (view _y ypr')) -- pitch
--             --     !*! fromQuaternion (axisAngle (view _z (view _m33 mtx0)) (view _z ypr')) -- roll
--             --   --tr  = DT.trace ("view translation mtx0: " ++ show (view (_w._xyz) mtx0)) $ view (_w._xyz) mtx0
--             --   --tr  = V3 0.777 0 0
--             --  tr  = view (_w._xyz) mtx0
--       --returnA -< obj0 { Object._transforms = (DT.trace ("mtx :" ++ show mtx) $ mtx) }
--       returnA -< obj0 { Object._transforms = mtx }

-- spin :: V3 Double -> V3 Double -> M44 Double -> M44 Double
-- spin pivot rot mtx = undefined

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


loadPreObjects :: ObjectClass -> Project -> [String]
loadPreObjects cls project = modelList
  where
    modelSet  = toListOf (models . traverse . path) project :: [String]
    modelList =
      
      case cls of
        Foreground -> (modelSet!!) <$> (concat $ toListOf ( objects . traverse . modelIDXs ) project)
        Background -> (modelSet!!) <$> (concat $ toListOf ( Project.background . traverse . modelIDXs ) project)
        Font       -> (toListOf (Project.fonts . traverse . path) project)
        _ -> undefined


loadObjects :: (([Int], Int, [Float], Material) -> IO Descriptor) -> Project -> IO ObjectTree
loadObjects initVAO project = 
  do
    -- _ <- Dt.trace ("project :" ++ show project) $ return ()
    print "Loading Models..."

    fgrVGeos  <-
      mapM (\modelPath ->
               do { vgeo <- readBGeo modelPath :: IO VGeo
                  ; return vgeo
                  }
           ) $ (loadPreObjects Foreground project ) :: IO [VGeo]

    bgrVGeos  <-
      mapM (\modelPath ->
               do { vgeo <- readBGeo modelPath :: IO VGeo
                  ; return vgeo
                  }
           ) $ (loadPreObjects Background project ) :: IO [VGeo]
                                                                                                                            
    fontsVGeos  <-
      mapM (\modelPath ->
               do { vgeo <- readBGeo modelPath :: IO VGeo
                  ; return vgeo
                  }
           ) $ (loadPreObjects Font project ) :: IO [VGeo]
                                                                                                                            
    objs  <- mapM (initObject project initVAO) fgrVGeos   :: IO [Object]
    bgrs  <- mapM (initObject project initVAO) bgrVGeos   :: IO [Object]
    fonts <- mapM (initObject project initVAO) fontsVGeos :: IO [Object]
    
    let result =
          ObjectTree
          ( GUI fonts [] )
          objs
          bgrs
    -- TODO : Here somewhere add solver inits <- read project file
    print "Finished loading models."
    
    return (result)


initObject :: Project -> (([Int], Int, [Float], Material) -> IO Descriptor) -> VGeo -> IO Object
initObject project initVAO vgeo =
  do
    print "Loading Materials..."
    mats  <- mapM readMaterial $ ms vgeo :: IO [Material]
    let (VGeo is_ st_ vs_ ms_ xf_) = vgeo
        vaoArgs = (\idx' st' vao' mat' ->  (idx', st', vao', mat')) <$.> is_ <*.> st_ <*.> vs_ <*.> mats
        offset = fmap ((view _w).fromList) (xf_)
        preTransforms = fmap fromList ((xf_))

    ds <- mapM initVAO vaoArgs

    progs <- mapM (\mat -> loadShaders
                           [ ShaderInfo VertexShader   (FileSource (_vertShader (mat) ))
                           , ShaderInfo FragmentShader (FileSource (_fragShader (mat) )) ]) mats

    let obj =
          defaultObj
          { _descriptors = ds
          , _materials   = mats
          , _programs    = progs
          , _transforms  = preTransforms
          -- , _pivot       = view _xyz offset
          -- , _solvers     =
          --   [(Rotate (view _xyz offset) (V3 0 0 1000))] -- TODO: a solver per ()transform) object
          --, _solvers = [(Rotate (V3 0 0 0) (V3 0 0 1000))] -- fmap (\offset' -> (Rotate (view _xyz offset') (V3 0 0 1000))) offset
          , Object._solvers = fmap fromString $
                         zip (concat $ toListOf (objects . traverse . (Project.solvers)) project :: [String])
                             (concat $ toListOf (objects . traverse . solverAttrs) project :: [[Int]])
          -- TODO : ^ init solvers <- read project file
          } :: Object

    return obj

fromVGeo :: (([Int], Int, [Float], Material) -> IO Descriptor) -> VGeo -> IO Object
fromVGeo initVAO (VGeo idxs st vaos matPaths xform) = 
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
          --, _transforms   = preTransform
          --, _pivot       = offset
          , Object._solvers     =
            [(Rotate (view _xyz offset) (V3 0 0 1000))]
          }

    return object
    
solve :: Object -> SF () Object
solve obj =
  proc () -> do
    mtxs <- (parB . fmap (transform obj)) slvs0 -< ()
    returnA -< obj { _transforms = vectorizedCompose mtxs }
      where
        slvs0 = view Object.solvers obj
        mtxs0 = view transforms     obj

transform :: Object -> Solver -> SF () ([M44 Double])
transform obj0 slv0 = 
  proc () ->
    do
      mtxs <- (parB . fmap (transformer slv0)) mtxs0 -< ()
      returnA -< mtxs
        where
          mtxs0 = view transforms obj0 :: [M44 Double]
          func  = undefined

vectorizedCompose :: [[M44 Double]] -> [M44 Double]
vectorizedCompose mtxss = 
  fmap (foldr1 (^*^)) $ DL.transpose mtxss

(^*^) :: M44 Double -> M44 Double -> M44 Double
(^*^) mtx0 mtx1 = mkTransformationMat rot tr
  where
    rot = (view _m33 mtx0) !*! (view _m33 mtx1) :: M33 Double
    tr  = (view translation mtx0) ^+^ (view translation mtx1)

-- foreach object:
--            \
--        foreach solver:
--              \              ...      \
--           foreach transform ... foreach property

-- TODO: [Object] -> [Solver] -> SF () [Object]
updateObjects :: [Object] -> SF () [Object]
updateObjects = parB . fmap solve
