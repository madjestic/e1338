{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Utils
  ( toIdxVAO
  , toIdxVAO'
  , Utils.fromList
  , (<$.>)
  , (<*.>)
  , toV3
  , rotateList
  , rotateList'
  , fromUUID
  , unsafeGenUUID
  , unsafeReGenUUID
  ) where

import Graphics.Rendering.OpenGL as GL (GLfloat)
import Data.Set                  as DS (fromList, toList)
import Data.List                 as DL (transpose)
import Data.List.Index                 (indexed)
import Data.List                       (elemIndex)
import Data.UUID                 as U
import Data.UUID.V4
import Data.Vector               as DV (fromList, (!), map, toList)
import Linear.V3
import Linear.V4
import Linear.Matrix
import Linear.Metric             as LM
import Data.VectorSpace          as DV
import Control.Lens ( view
                    , over
                    , traverse)
import System.IO.Unsafe

import Debug.Trace as DT

instance VectorSpace (V3 Double) Double where
  zeroVector                   = (V3 0 0 0)
  (*^) s (V3 x y z)            = (V3 (s*x) (s*y) (s*z))
  (^+^)  (V3 x y z) (V3 k l m) = (V3 (x+k) (y+l) (z+m))
  dot    (V3 x y z) (V3 k l m) = (x*k) + (y*l) + (z*m)

instance VectorSpace (V4 (V4 Double)) Double where
  zeroVector                   = identity :: M44 Double
  (*^) s (m :: M44 Double)     = m !!* s
  (^+^)  (m :: M44 Double) (n :: M44 Double) = 
    mkTransformationMat
    rot
    tr
     where
      rot = LM.normalize $ (inv33 m') !*! (n')
        where
          m' = view _m33 m
          n' = view _m33 n
      tr = (view translation m) ^+^ (view translation n)
          
  dot    (m :: M44 Double) (n :: M44 Double) = DV.dot m n

-- | [Float]  ~= vertex
--  [[Float]] ~= VAO
toIdxVAO :: Int -> [[Float]] -> ([Int],[Float])
toIdxVAO stride vao = (idx, idxVAO)
  where
    iListSet = indexed $ DS.toList $ DS.fromList $ vao                       :: [(Int,[Float])]
    iList    = indexed vao                                                   :: [(Int, [GLfloat])]
    idx      = fmap (\(i,_) -> (fromIntegral i)) (matchLists iListSet iList) :: [Int]
    idxVAO   = concat $ fmap (\x -> snd x) iListSet                          :: [Float]

toIdxVAO' :: Int -> [[Float]] -> ([Int],[Float])
toIdxVAO' stride vao = (idx, idxVAO)
  where
    --iListSet = indexed $ DS.toList $ DS.fromList $ vao                       :: [(Int,[Float])]
    iList    = indexed vao                             :: [(Int, [GLfloat])]
    idx      = fmap (\(i,_) -> (fromIntegral i)) iList :: [Int]
    idxVAO   = concat $ fmap (\x -> snd x) iList       :: [Float]

-- | matchLists - cross-match 2 listst, replacing the elements of list2 with matching
-- |          with elements of list1, concatenating the non-matched elements.
-- |   il - indexed list
-- |  nil - non-indexed list
matchLists :: [(Int, [GLfloat])] -> [(Int, [GLfloat])] -> [(Int, [GLfloat])]
matchLists il nil =
  fmap (mFunc il ) nil -- | mFunc - matching function
  where
    -- | il      - indexed list
    -- | nile    - non indexed list element
    -- | Replaces the target element with the first match from the matching list il
    il' = DV.fromList il
    cxs'  = DV.map (\(i,s) -> s) il'
    mFunc il nile@(iy, cy) =
      (\x -> case x of
               Just idx -> il' ! idx
               Nothing  -> (-iy, cy) ) nili -- | if a unique index idx found - flip the sign
                                            -- | the idea idx to separate normal indexes
                                            -- | and unique indexes -> [idx:uidx] later
      where
        nili = elemIndex cy (DV.toList cxs')
        -- cxs  = DV.map (\(i,s) -> s) il' -- :: [[GLfloat]]

-- TODO: create a fromList typeclass?
-- [a] -> V3 a
-- [a] -> M44 a
-- etc.
fromList :: [Float] -> M44 Double
fromList xs' = V4 x y z w
  where
    x  = V4 (xs!!0 ) (xs!!1 ) (xs!!2 ) (xs!!3 ) 
    y  = V4 (xs!!4 ) (xs!!5 ) (xs!!6 ) (xs!!7 ) 
    z  = V4 (xs!!8 ) (xs!!9 ) (xs!!10) (xs!!11) 
    w  = V4 (xs!!12) (xs!!13) (xs!!14) (xs!!15)
    xs = fmap realToFrac xs' :: [Double]

(<$.>) :: (a -> b) -> [a] -> [b]
(<$.>) = fmap

(<*.>) :: [a -> b] -> [a] -> [b]
(<*.>) = zipWith ($)

toV3 :: [a] -> V3 a
toV3 xs = V3 (xs!!0) (xs!!1) (xs!!2)

rotateList :: Int -> [a] -> [a]
rotateList _ [] = []
rotateList n xs = zipWith const (drop n (cycle xs)) xs

rotateList' :: (Int, [a]) -> [a]
rotateList' (_, []) = []
rotateList' (n, xs) = zipWith const (drop n (cycle xs)) xs

fromUUID :: UUID -> Integer
fromUUID x = read $ concatMap show $ (\ (x,y,z,w)-> fmap toInteger [x,y,z,w]) $ toWords x

-- unsafeGenUUID :: UUID -> UUID
-- unsafeGenUUID uuid =
--   case (U.null uuid) of
--     True -> unsafePerformIO nextRandom
--     _    -> uuid

unsafeReGenUUID :: UUID -> UUID
unsafeReGenUUID uuid = unsafePerformIO nextRandom

unsafeGenUUID :: UUID
unsafeGenUUID = unsafePerformIO nextRandom


-- over (objects . traverse . objID) unsafeGenUUID defaultProject
-- read $ concatMap show [1,2,3] :: Integer
-- read $ concatMap show $ (\ (x,y,z,w)-> fmap toInteger [x,y,z,w]) $ toWords $ unsafePerformIO nextRandom :: Integer
-- take 3 . repeat $ unsafePerformIO nextRandom
