--  .d8888b. 8888888888 .d88888b. 888b     d8888888888888888888888888888888b.Y88b   d88P 
-- d88P  Y88b888       d88P" "Y88b8888b   d8888888           888    888   Y88bY88b d88P  
-- 888    888888       888     88888888b.d88888888           888    888    888 Y88o88P   
-- 888       8888888   888     888888Y88888P8888888888       888    888   d88P  Y888P    
-- 888  88888888       888     888888 Y888P 888888           888    8888888P"    888     
-- 888    888888       888     888888  Y8P  888888           888    888 T88b     888     
-- Y88b  d88P888       Y88b. .d88P888   "   888888           888    888  T88b    888     
--  "Y8888P888888888888 "Y88888P" 888       8888888888888    888    888   T88b   888     

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module PGeo
  ( PGeo(..)
  , VGeo(..)
  , Vec3
  , readPGeo
  , readVGeo
  , readBGeo
  , fromPGeo
  , fromPGeo'
  ) where

import Data.Aeson
import Data.Aeson.TH
import Data.Maybe                             (fromMaybe)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString      as BS
import Graphics.Rendering.OpenGL      as GL   (Vertex4(..))
import Data.Store                     as DS

import FromVector
import Utils
import VAO

-- import Debug.Trace   as DT

-- | TODO : replace Vec3 -> Vec4
type Vec3 = (Double, Double, Double)

type Vec4 = (Double, Double, Double, Double)

instance FromVector Vec3 where
  toVertex4 :: Vec3 -> Vertex4 Double
  toVertex4 (k, l, m) = Vertex4 k l m 1.0  

data PGeo
  =  PGeo
     {
       ids   :: [[Int]]  
     , as    :: [Float] -- 1
     , cs    :: [Vec3]  -- 3
     , ns    :: [Vec3]  -- 3
     , uvws  :: [Vec3]  -- 3
     , ps    :: [Vec3]  -- 3 -> 13+1 -> 14 stride -- TODO: add v (vel), m (mass)
     , mats  :: [String]
     , m     :: [Float]
     , v     :: [Vec3] -- [[x,y,x]] ~= [Vec3]
     , xform :: [[Float]] -- [xform M44]
     }
  deriving Show
deriveJSON defaultOptions ''PGeo

data VGeo
  =  VGeo
     {
       is  :: [[Int]]    -- indices
     , st  :: [Int]      -- strides
     , vs  :: [[Float]]  -- all attrs as a flat list
     , mts :: [FilePath] -- materials
     , ms  :: [Float]    -- masses
     , vls :: [[Float]]     -- velocities
     , xf  :: [[Float]]  -- preTransforms
     }
  deriving Show

readBGeo :: FilePath -> IO VGeo
readBGeo file = 
  do
    -- _ <- DT.trace "trace" $ return ()
    bs <- BS.readFile file
    return $ case (DS.decode bs) of
               Right (idxs, st, vaos, mats, mass, vels, xform) -> VGeo idxs st vaos mats mass vels xform
               Left _ -> VGeo [[]] [] [[]] [] [] [] [[]]

readVGeo :: FilePath -> IO VGeo
readVGeo file = 
  do
    d <- decodeFileStrict file :: IO (Maybe ([[Int]],[Int],[[Float]],[String], [Float], [[Float]], [[Float]]))
    return $ case d of
               Just (idxs, st, vaos, mats, mass, vels, xform) -> VGeo idxs st vaos mats mass vels xform
               Nothing  -> VGeo [[]] [] [[]] [] [] [] [[]]

readPGeo :: FilePath -> IO PGeo
readPGeo jsonFile =
  do
    d <- (eitherDecode <$> BL.readFile jsonFile) :: IO (Either String PGeo)
    --print $ "readPGeo.d :" ++ show d
    -- case d of
    _ <-
      case (fromEither d) of
        Nothing -> print $ "readPGeo.d :" ++ show d
        _ -> putStrLn $ "parsing the " ++ (show jsonFile) ++ ": OK"
    
    let ids'  = (ids   . fromEitherDecode) d
        as'   = (as    . fromEitherDecode) d
        cs'   = (cs    . fromEitherDecode) d
        ns'   = (ns    . fromEitherDecode) d
        uvws' = (uvws  . fromEitherDecode) d
        ps'   = (ps    . fromEitherDecode) d
        mts'  = (mats  . fromEitherDecode) d
        mass' = (m     . fromEitherDecode) d
        vels' = (v     . fromEitherDecode) d
        xf'   = (xform . fromEitherDecode) d
    return $ PGeo ids' as' cs' ns' uvws' ps' mts' mass' vels' xf'

      where
        fromEitherDecode = fromMaybe (PGeo [[]] [] [] [] [] [] [] [] [] [[]]) . fromEither
        fromEither d =
          case d of
            Right pt -> Just pt
            _ -> Nothing

fromPGeo :: PGeo -> VGeo
fromPGeo (PGeo idx' as' cs' ns' uvw' ps' mts' mass' vels' xf') = (VGeo idxs st vaos mts' mass' vels xf')
  where
    stride = 13 -- TODO: make it more elegant, right now VBO's are hard-coded to be have stride = 13...
    vao = (toVAO idx' as' cs' ns' uvw' ps')
    (idxs, vaos) = unzip $ fmap (toIdxVAO stride) vao -- that already outputs [[]], but vao, I think,is still a single element list?
    st           = take (length vaos) $ repeat stride
    vels         = fmap (\(x,y,z)   -> fmap realToFrac [x,y,z]) vels'

fromPGeo' :: PGeo -> VGeo
fromPGeo' (PGeo idx' as' cs' ns' uvw' ps' mts' mass' vels' xf') = (VGeo idxs st vaos mts' mass' vels xf')
  where
    stride = 13 -- TODO: make it more elegant, right now VBO's are hard-coded to be have stride = 13...
    vao = (toVAO idx' as' cs' ns' uvw' ps')
    (idxs, vaos) = unzip $ fmap (toIdxVAO' stride) vao -- that already outputs [[]], but vao, I think,is still a single element list?
    st           = take (length vaos) $ repeat stride
    vels         = fmap (\(x,y,z)   -> fmap realToFrac [x,y,z]) vels'
