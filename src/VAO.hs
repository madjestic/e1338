module VAO
  (
    VAO
  , toVAO
  ) where

-- import Data.Vector as V
import Data.Massiv.Array as A
import GHC.Float

-- import Debug.Trace   as DT

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

    indices = fromLists' Par idxs :: (Array U Ix2 Int)
    as'' = fromLists' Par as'    :: (Array U Ix2 Float)
    cds''= fromLists' Par cds'   :: (Array U Ix2 Float)
    ns'' = fromLists' Par ns'    :: (Array U Ix2 Float)
    ts'' = fromLists' Par ts'    :: (Array U Ix2 Float)
    ps'' = fromLists' Par ps'    :: (Array U Ix2 Float)

    --cList' = DT.trace "Making cList" $ toLists2 . computeAs U $ concat' 1 [as'', cds'', ns'', ts'', ps''] :: [[Float]]
    cList' = toLists2 . computeAs U $ concat' 1 [as'', cds'', ns'', ts'', ps''] :: [[Float]]
    
    mat = fromLists' Par cList' :: (Array U Ix2 Float)
    cListOpt =
      toLists2 . computeAs P <$>
      fmap (\row -> backpermute' (Sz (Prelude.length (idxs !! row) :. 13)) (\(i :. j) -> ((indices !> row) ! i) :. j) mat) [0 .. div (elemsCount indices) (elemsCount (indices !> 0))-1]
    --vaos = (DT.trace ("cListOpt" ++ show cListOpt) $ cListOpt)
    vaos = cListOpt
