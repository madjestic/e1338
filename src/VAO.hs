module VAO
  (
    VAO (..)
  , toVAO
  ) where

-- import Data.Vector as V
import Data.Massiv.Array as A
import GHC.Float
import Unsafe.Coerce

import Utils ((<$.>), (<*.>))

import Debug.Trace   as DT

type VAO = [[[Float]]]

toVAO
  :: [[Int]]
  -> [Float]
  -> [(Double, Double, Double)]
  -> [(Double, Double, Double)]
  -> [(Double, Double, Double)]
  -> [(Double, Double, Double)]
  -> VAO

toVAO idxs as cds ns ts ps = vaos
  where
    as'  = fmap (\a -> [a]) as                                :: [[Float]]
    cds' = fmap (\(r,g,b)   -> fmap double2Float [r,g,b]) cds :: [[Float]]
    ns'  = fmap (\(x,y,z)   -> fmap double2Float [x,y,z]) ns
    ts'  = fmap (\(u,v,w)   -> fmap double2Float [u,v,w]) ts
    ps'  = fmap (\(x,y,z)   -> fmap double2Float [x,y,z]) ps

    -- as'  = unsafeCoerce as :: [[Float]]
    -- cds' = unsafeCoerce cds:: [[Float]] 
    -- ns'  = unsafeCoerce ns :: [[Float]] 
    -- ts'  = unsafeCoerce ts :: [[Float]]  
    -- ps'  = unsafeCoerce ps :: [[Float]]  

    --idxsArr = fromLists' Par idxs :: (Array P Ix2 Int)

    indices = fromLists' Par idxs :: (Array U Ix2 Int)
    as'' = fromLists' Par as'    :: (Array U Ix2 Float)
    cds''= fromLists' Par cds'   :: (Array U Ix2 Float)
    ns'' = fromLists' Par ns'    :: (Array U Ix2 Float)
    ts'' = fromLists' Par ts'    :: (Array U Ix2 Float)
    ps'' = fromLists' Par ps'    :: (Array U Ix2 Float)

    cList = DT.trace "Making cList" $ (\as'' cds'' ns'' ts'' ps'' -> Prelude.concat [as'', cds'', ns'', ts'', ps''])
            <$.> as' <*.> cds' <*.> ns' <*.> ts' <*.> ps'  :: [[Float]]

    cList' = DT.trace "Making cList" $ toLists2 . computeAs U $ concat' 1 [as'', cds'', ns'', ts'', ps''] :: [[Float]]
    
    -- cList = DT.trace "Making cList" $ (\as'' cds'' ns'' ts'' ps'' -> concat [as'', cds'', ns'', ts'', ps''])
    --         <$.> (DT.trace ("as' :" ++ show as') $ as' )
    --         <*.> (DT.trace ("cds'" ++ show cds') $ cds') 
    --         <*.> (DT.trace ("ns' " ++ show ns' ) $ ns' ) 
    --         <*.> (DT.trace ("ts' " ++ show ts' ) $ ts' ) 
    --         <*.> (DT.trace ("ps' " ++ show ps' ) $ ps' ) :: [[Float]]

    mat = fromLists' Par cList' :: (Array U Ix2 Float)
    --vaos' = computeAs U $ backpermute' (Sz (13 :. (Prelude.length idxs))) (\(i :. j) -> (indices ! i) :. j) mat
    --cListOpt = fmap (toLists2 . computeAs P) $ fmap (\row -> backpermute' (Sz (4 :. (Prelude.length (idxs !! row)))) (\(i :. j) -> ((indices !> row) ! i) :. j) mat) [0 .. div (elemsCount indices) (elemsCount (indices !> 0))-1]
    cListOpt = fmap (toLists2 . computeAs P) $ fmap (\row -> backpermute' (Sz ((Prelude.length (idxs !! row)) :. 13)) (\(i :. j) -> ((indices !> row) ! i) :. j) mat) [0 .. div (elemsCount indices) (elemsCount (indices !> 0))-1]
    cListNOpt  = fmap (\idx -> (fmap (\i -> cList!!i) idx)) idxs
    --vaos = (DT.trace ("cListOpt" ++ show cListOpt) $ cListOpt)
    vaos = cListOpt
    
    --vaos = (DT.trace ("cListNOpt" ++ show cListNOpt) $ cListNOpt)
    --vaos = cListNOpt

    -- vaos = [cList']
    --vaos  = toLists2 $ DT.trace "Indexing cList" $ fmap (\idx -> (fmap (\i -> arrList (A.!) i) idx)) indices :: _ --VAO

-- | fromListsM Par ([[0,0,0,0,0],[0,1,2,3,4],[0,2,4,6,8]]) :: Maybe (Array P Ix2 Int)
-- Just (Array P Par (Sz (3 :. 5))
--   [ [ 0, 0, 0, 0, 0 ]
--   , [ 0, 1, 2, 3, 4 ]
--   , [ 0, 2, 4, 6, 8 ]
--   ]
-- )

-- backpermute' @P (Sz1 3) (idxs !) idxs -- seems to work
-- backpermute' @P (Sz1 3) ((fromLists' Par ([0,1,2]) :: (Array P Ix1 Int)) !) idxs
-- backpermute' @P (Sz (3 :. 4)) ((fromLists' Par ([0,1,2]) :: (Array P Ix1 Int)) !>) (fromLists' Par ([[0,0,0,0,0],[0,1,2,3,4],[0,2,4,6,8]]) :: (Array P Ix2 Int))
-- fromLists' Par ([[0,0,0,0,0],[0,1,2,3,4],[0,2,4,6,8]]) :: (Array P Ix2 Int)
-- map (listArray (0,2) [3,4,5] !) [0,2,1,0]`
-- backpermute' @P (Sz1 4) ((fromLists' Par ([0,2,1,0]) :: (Array P Ix1 Int)) !) (fromLists' Par ([3,4,5]) :: (Array P Ix1 Int))
-- elems (ixmap (0,3) (listArray (0,3) [0,2,1,0] !) (listArray (0,2) [3,4,5]))  -- madjestic, yea, looks like this

-- as' = [[0.999],[0.999],[0.999]]
-- cds' = [[0.48600417,0.45986164,0.5334139],[0.8073105,0.77029485,0.7923097],[0.48891538,0.5467133,0.46862027]]
-- ns' = [[-4.297911e-2,0.43491858,-0.8994435],[-4.297911e-2,0.43491858,-0.8994435],[-4.297911e-2,0.43491858,-0.8994435]]
-- ts' = [[0.6743014,0.16806507,0.94919837],[0.5278542,0.42955464,1.0204161],[6.512364e-2,0.70833755,1.3468704]]
-- ps' = [[-2.9026437e12,1.064354e13,1.5896469e12],[-1.0516166e13,4.68407e12,-9.282008e11],[9.09026e12,-5.7552e12,-6.912901e12]]

-- as'' = fromLists' Par as' :: (Array P Float)
-- cds'' = fromLists' Par cds' :: (Array P Float)
-- ns'' = fromLists' Par ns' :: (Array P Float)
-- ts'' = fromLists' Par ts' :: (Array P Float)
-- ps'' = fromLists' Par ps' :: (Array P Float)

-- concat' 1 [as'', cds'', ns'', ts'', ps'']

-- mList = [(0, [1.0, 2.0])] :: [(Int, [Float])]

-- let mat = fromLists' Par ([[3,4,5],[6,7,8],[10,11,12]]) :: (Array P Ix2 Int)
-- let indices = fromList Par [0,2,1] :: (Array P Ix1 Int)
-- let indices2 = fromLists' Par [[0,2,1],[0,1,2]] :: (Array P Ix1 Int)
-- mat'' <- stackOuterSlicesM $ A.map (mat !>) indices2 :: IO (Matrix DL Int)

-- concat' 2 $ fmap (\row -> backpermute' (Sz (3 :. 3)) (\(i :. j) -> ((indices2 !> row) ! i) :. j) mat) [0,1]
-- div (elemsCount indices2) (elemsCount (indices2 !> 0))
-- [0 .. div (elemsCount indices2) (elemsCount (indices2 !> 0))-1]

-- arrays = fmap (\row -> backpermute' (Sz (3 :. 3)) (\(i :. j) -> ((indices2 !> row) ! i) :. j) mat) [0 .. div (elemsCount indices2) (elemsCount (indices2 !> 0))-1]
-- fmap (toLists2 . computeAs P) arrays
