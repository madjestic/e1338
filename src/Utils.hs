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
  , encodeStringUUID
  ) where

import Control.Lens ( view
                    , over
                    , traverse)
import Graphics.Rendering.OpenGL as GL (GLfloat)
import Data.ByteString.Char8           (pack
                                       ,unpack)
import Data.Set                  as DS (fromList, toList)
import Data.List                 as DL (transpose)
import Data.List.Index                 (indexed)
import Data.List                       (elemIndex)
import Data.Locator
import Data.UUID                 as U
import Data.UUID.V4
import Data.Vector               as DV (fromList, (!), map, toList)
import Data.VectorSpace          as DV
import Graphics.Rendering.OpenGL (GLuint)
import Linear.V3
import Linear.V4
import Linear.Matrix
import Linear.Metric             as LM
import System.IO.Unsafe
import System.Random
import Unsafe.Coerce

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
toV3 xs = V3 (head xs) (xs!!1) (xs!!2)

rotateList :: Int -> [a] -> [a]
rotateList _ [] = []
rotateList n xs = zipWith const (drop n (cycle xs)) xs

rotateList' :: (Int, [a]) -> [a]
rotateList' (_, []) = []
rotateList' (n, xs) = zipWith const (drop n (cycle xs)) xs

fromUUID :: UUID -> GLuint
fromUUID x = read $ concatMap show $ (\ (x,y,z,w)-> fmap toInteger [x,y,z,w]) $ toWords x

-- | Generate a UUID, based on FilePath
-- | e.g. fromUUID $ encodeStringUUID "./projects/.temp1"
-- | > 2836415114
encodeStringUUID :: String -> UUID
encodeStringUUID x = genSeedUUID . fromInteger . fromBase62 . unpack . hashStringToBase62 6 $ pack x

encodeStringInteger :: String -> Integer
encodeStringInteger x = fromBase62 . unpack . hashStringToBase62 1 $ pack x

genSeedUUID :: Int -> UUID
genSeedUUID seed =
  let
      g0 = mkStdGen seed -- RNG from seed
      (u1, g1) = random g0
  in u1

