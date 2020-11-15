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


loadPreObjects :: ObjectClass -> Project -> [String]
loadPreObjects cls project = modelList
  where
    modelSet  = toListOf (models . traverse . path) project :: [String]
    modelList =
      
      case cls of
        Foreground -> (modelSet!!) <$> (concat $ toListOf ( objects . traverse . modelIDXs ) project)
        Background -> (modelSet!!) <$> (concat $ toListOf ( Project.background . traverse . modelIDXs ) project)
        Font       -> (toListOf (Project.fonts . traverse . path) project)

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
                                                                                                                            
    fntVGeos  <-
      mapM (\modelPath ->
               do { vgeo <- readBGeo modelPath :: IO VGeo
                  ; return vgeo
                  }
           ) $ (loadPreObjects Font project ) :: IO [VGeo]
                                              
    -- print $ "loadObjects fgrVGeos :" ++ show (length fgrVGeos)
    -- print $ "loadObjects bgrVGeos :" ++ show (length bgrVGeos)
    -- print $ "loadObjects fntVGeos :" ++ show (length fntVGeos)
    objs  <- if (length fgrVGeos > 0 ) then (mapM (initObject project initVAO Foreground) $ zip fgrVGeos [0..]) else pure []  :: IO [Object]
    bgrs  <- if (length bgrVGeos > 0 ) then (mapM (initObject project initVAO Background) $ zip bgrVGeos [0..]) else pure []  :: IO [Object]
    fonts <- if (length fntVGeos > 0 ) then (mapM (initObject project initVAO Font)       $ zip fntVGeos [0..]) else pure []  :: IO [Object]
    --let
    --   objs = []  :: [Object] 
      -- bgrs = []  :: [Object] 
      -- fonts = [] :: [Object] 
    -- print $ "loadObjects objs :" ++ show objs
    
    let result =
          ObjectTree
          ( GUI fonts [] )
          objs
          bgrs
    -- TODO : Here somewhere add solver inits <- read project file
    print "Finished loading models."
    
    return (result)


initObject :: Project -> (([Int], Int, [Float], Material) -> IO Descriptor) -> ObjectClass -> (VGeo, Int) -> IO Object
initObject project initVAO cls (vgeo, idx) =
  do
    print "Loading Materials..."
    mats  <- mapM readMaterial $ ms vgeo :: IO [Material]
    let (VGeo is_ st_ vs_ ms_ xf_) = vgeo
        vaoArgs = (\idx' st' vao' mat' ->  (idx', st', vao', mat')) <$.> is_ <*.> st_ <*.> vs_ <*.> mats
        offset = fmap ((view _w).fromList) (xf_)
        preTransforms = fmap fromList ((xf_))

    -- print $ "preTransforms :" ++ show preTransforms
    ds <- mapM initVAO vaoArgs

    progs <- mapM (\mat -> loadShaders
                           [ ShaderInfo VertexShader   (FileSource (_vertShader (mat) ))
                           , ShaderInfo FragmentShader (FileSource (_fragShader (mat) )) ]) mats
    -- print $ "initObject idx:" ++ show idx
    let obj =
          defaultObj
          { _descriptors = ds
          , _materials   = mats
          , _programs    = progs
          , _transforms  = preTransforms
          -- TODO: separate solvers per fgr,bgr,fonts. Current solution works for fgr objects, but not for fonts, etc.
          , Object._solvers = fmap fromString $
                         -- TODO: error is somewhere here: one solver gets copied multiple times (by the number of objects, it seems...)
                         -- zip (concat $ toListOf (objects . traverse . (Project.solvers)) project :: [String])
                         --     (concat $ toListOf (objects . traverse . solverAttrs) project :: [[Int]])
                         -- zip (((toListOf (objects . traverse . (Project.solvers)) project)!!idx) :: [String])
                         --     (((toListOf (objects . traverse . solverAttrs) project)!!idx) :: [[Int]])
                              zip solvers' attrs'
          } :: Object

    return obj
      where
        solvers' =
          case cls of
            Foreground -> (concat $ toListOf (objects            . traverse . (Project.solvers)) project :: [String])
            Background -> (concat $ toListOf (Project.background . traverse . (Project.solvers)) project :: [String])
            Font       -> []
        attrs'   =
          case cls of
            Foreground -> (((toListOf (objects . traverse . solverAttrs) project)!!idx) :: [[Int]])
            Background -> (((toListOf (Project.background . traverse . solverAttrs) project)!!idx) :: [[Int]])
            Font       -> []

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
          -- , Object._solvers =
          --   [(Rotate (view _xyz offset) (V3 0 0 1000))]
          }

    return object
    
solve :: Object -> SF () Object
solve obj =
  proc () -> do
    -- let
    --   trs =
    --     [V4
    --       (V4 1.0 0.0 0.0 0.0)
    --       (V4 0.0 1.0 0.0 0.0)
    --       (V4 0.0 0.0 1.0 1.49999992832e11)
    --       (V4 0.0 0.0 0.0 1.0)]
    
    --mtxs <- (parB . fmap (transform obj)) (DT.trace ("solve slvs0 :" ++ show slvs0)$ slvs0) -< ()
    mtxs <- (parB . fmap (transform obj)) slvs0 -< ()
    --returnA -< (DT.trace ("solve mtxs :" ++ show (vectorizedCompose mtxs)) $ obj { _transforms = vectorizedCompose mtxs })    
    returnA -< obj { _transforms = vectorizedCompose mtxs }
    --returnA -< obj { _transforms = trs }
      where
        slvs0 = view Object.solvers obj
        mtxs0 = view transforms     obj

transform :: Object -> Solver -> SF () ([M44 Double])
transform obj0 slv0 = 
  proc () ->
    do
      -- let
      --   trs =
      --     V4
      --       (V4 1.0 0.0 0.0 0.0)
      --       (V4 0.0 1.0 0.0 0.0)
      --       (V4 0.0 0.0 1.0 1.49999992832e11)
      --       (V4 0.0 0.0 0.0 1.0)
      --   id = identity :: M44 Double
      --   mtxs' = [trs, id]  
        
      mtxs <- (parB . fmap (transformer slv0)) mtxs0 -< ()
      -- returnA -< (DT.trace ("transform mtxs :" ++ show mtxs)$ mtxs)      
      returnA -< mtxs
        where
          mtxs0 = view transforms obj0 :: [M44 Double]
          func  = undefined

-- TODO: here's a glitch
vectorizedCompose :: [[M44 Double]] -> [M44 Double]
vectorizedCompose mtxss =
  -- [V4
  --   (V4 1.0 0.0 0.0 0.0)
  --   (V4 0.0 1.0 0.0 0.0)
  --   (V4 0.0 0.0 1.0 1.49999992832e11)
  --   (V4 0.0 0.0 0.0 1.0)]
  fmap (foldr1 (^*^)) $ DL.transpose mtxss
  --fmap (foldr (^*^) (identity :: M44 Double)) $ DL.transpose (DT.trace ("vectorizedCompose mtxss :" ++ show mtxss) $ mtxss)

(^*^) :: M44 Double -> M44 Double -> M44 Double
(^*^) mtx0 mtx1 = mkTransformationMat rot tr
  where
    rot = (view _m33 mtx0) !*! (view _m33 mtx1) :: M33 Double
    tr  = (view translation mtx0) ^+^ (view translation mtx1)

-- (^*^) :: M44 Double -> M44 Double -> M44 Double
-- (^*^) mtx0 mtx1 =
--   V4
--     (V4 1.0 0.0 0.0 0.0)
--     (V4 0.0 1.0 0.0 0.0)
--     (V4 0.0 0.0 1.0 1.49999992832e11)
--     (V4 0.0 0.0 0.0 1.0)
  
    

-- foreach object:
--            \
--        foreach solver:
--              \              ...      \
--           foreach transform ... foreach property

-- TODO: [Object] -> [Solver] -> SF () [Object]
updateObjects :: [Object] -> SF () [Object]
updateObjects =  parB . fmap solve
-- updateObjects objs =
--   proc () -> 
--     do
--       let
--         trs =
--           [V4
--             (V4 1.0 0.0 0.0 0.0)
--             (V4 0.0 1.0 0.0 0.0)
--             (V4 0.0 0.0 1.0 1.49999992832e11)
--             (V4 0.0 0.0 0.0 1.0)]
--         objs' = fmap (\obj -> obj { _transforms = trs }) objs
--       returnA -< objs'
      
        -- result = [Object
        --           { _descriptors =
        --             [Descriptor (VertexArrayObject {vertexArrayID = 14}) 3450]
        --           , _materials =
        --             [Material { Material._name = "Earth"
        --                       , _vertShader = "./mat/earth/Earth/shader.vert"
        --                       , _fragShader = "./mat/earth/Earth/shader.frag"
        --                       , _textures = ["textures/earth_daymap_4096.jpg"]}]
        --           , _programs = [Program {programID = 40}]
        --           , _transforms =
        --             [V4
        --              (V4 1.0 0.0 0.0 0.0)
        --              (V4 0.0 1.0 0.0 0.0)
        --              (V4 0.0 0.0 1.0 1.49999992832e11)
        --              (V4 0.0 0.0 0.0 1.0)]
        --           , _velocity = V3 0.0 0.0 0.0
        --           , _avelocity = V3 0.0 0.0 0.0
        --           , _mass = 1.0
        --           , _density = 1.0
        --           , _time = 0.0
        --           , Object._solvers =
        --             [Rotate { _pivot = V3 0.0 0.0 0.0
        --                     , _ypr = V3 0.0 0.0 0.0}]}]
