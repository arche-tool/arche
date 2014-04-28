{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE FlexibleInstances #-}

module Gamma.Strategy.Gomes
       ( run ) where

import qualified Data.Vector                  as V
import qualified Data.Vector.Unboxed          as U
import qualified Data.Vector.Unboxed.Mutable  as MU
import qualified Data.HashMap.Strict          as HM
import qualified Data.HashSet                 as HS

import           Data.Vector.Unboxed (Vector)
import           Data.HashMap.Strict (HashMap)
import           Data.HashSet        (HashSet)
import           Data.Maybe          (mapMaybe)

import           System.FilePath
import           Control.Parallel.Strategies

import           Hammer.Math.Algebra         (Vec3(..), Vec4(..))
import           Hammer.VoxBox
import           Hammer.VTK.VoxBox
import           Hammer.VTK
import           Hammer.MicroGraph

import           Texture.Symmetry            (Symm (..))
import           Texture.Orientation
import           File.ANGReader

import           Gamma.Grains
import           Gamma.OR

import Debug.Trace
dbg a = trace (show a) a
dbgs s a = trace (show s ++ " <=> " ++ show a) a

run :: Deg -> FilePath -> FilePath -> IO ()
run miso fin fout = do
  ang <- parseANG fin
  let vbq = ebsdToVoxBox ang rotation
  case getGrainID miso Cubic vbq of
    Nothing               -> print "No grain detected!"
    Just gidMap@(vbgid, _) -> let
      realOR = fromQuaternion $ mkQuaternion $ Vec4 7.126e-1 2.895e-1 2.238e-1 5.986e-1
      mkr    = fst $ getMicroVoxel gidMap
      fs     = findConnFaces vbq mkr realOR -- or ksOR
      ggh    = grainsGraph fs
      vbgid2 = gammaBox gidMap ggh
      vtkGB  = renderGB   vbq mkr fs
      vtkBox = renderVoxBoxVTK vbgid2 [cell1, cell2]
      cell1  = mkCellAttr "GammaGB" (\i _ _ -> (grainID vbgid2) U.! i)
      cell2  = mkCellAttr "AlphaGB" (\i _ _ -> unGrainID $ (grainID vbgid)  U.! i)
      in do
        print $ grainsGraph fs
        writeUniVTKfile (fout <.> "vtu") True vtkGB
        writeUniVTKfile (fout <.> "vtr") True vtkBox

gammaBox :: (VoxBox GrainID, HashMap Int (V.Vector VoxelPos)) -> [HashSet Int] -> VoxBox Int
gammaBox (vb@VoxBox{..}, gidMap) gs = let
  getIS oldID = maybe (V.empty) (V.map (dimension %@)) (HM.lookup oldID gidMap)
  func v (newID, s) = mapM_ (V.mapM_ (\i -> MU.write v i newID) . getIS) (HS.toList s)
  vec = U.create $ do
    v <- MU.replicate (U.length grainID) (-1)
    mapM_ (func v) (zip [1..] gs)
    return v
  in vb {grainID = vec}

grainsGraph :: [FaceID] -> [HashSet Int]
grainsGraph = connComp . mkGraph . map unFaceID

findConnFaces :: VoxBox Quaternion -> MicroVoxel -> OR -> [FaceID]
findConnFaces vbq micro withOR = let
  es  = HM.keys $ microEdges micro
  ors = genTS withOR
  bs  = map (testEdge vbq micro ors) es
  fs  = bs `using` parListChunk 100 rpar
  in concatMap (\(f1, f2, f3)-> [f1, f2, f3]) (mapMaybe id fs)

testEdge :: VoxBox Quaternion -> MicroVoxel -> Vector OR ->
            EdgeID -> Maybe (FaceID, FaceID, FaceID)
testEdge vbq micro ors eid = let
  func = testFace vbq micro ors
  in case unEdgeID eid of
  Left (f1, f2, f3)
    | func f1 &&
      func f2 &&
      func f3 -> return (f1, f2, f3)
  _ -> Nothing

testFace :: VoxBox Quaternion -> MicroVoxel -> Vector OR -> FaceID -> Bool
testFace vbq micro ors fid = let
  facelist = getFaceProp fid micro >>= getPropValue
  func fs = let
    bs = V.filter id $ V.map (testSingleFace vbq ors) fs
    fn = fromIntegral $ V.length fs
    on = fromIntegral $ V.length bs
    in dbgs fid $ (on / fn) :: Double
  in maybe False ((> 0.5) . func) facelist

testSingleFace :: VoxBox Quaternion -> Vector OR -> FaceVoxelPos -> Bool
testSingleFace vbq ors face = let
  (v1, v2) = getFaceVoxels face
  q1 = vbq #! v1
  q2 = vbq #! v2
  in (fromAngle $ Deg 3) > misoOR ors Cubic q1 q2

renderGB :: VoxBox Quaternion -> MicroVoxel -> [FaceID] -> VTK Vec3
renderGB vb micro fs = addData $ renderAllElemProp vb fprop
  where
    fprop = mapMaybe ((flip getFaceProp) micro) fs
    addData vtk = let
      func _ _ _ = 1 :: Int
      in addDataCells vtk (mkCellAttr "ConnGB" func)

-- TODO move to Hammer
-- | Get both voxels that forms a given face.
getFaceVoxels :: FaceVoxelPos -> (VoxelPos, VoxelPos)
getFaceVoxels (Fx pos) = (pos, pos #+# (VoxelPos (-1) 0 0))
getFaceVoxels (Fy pos) = (pos, pos #+# (VoxelPos 0 (-1) 0))
getFaceVoxels (Fz pos) = (pos, pos #+# (VoxelPos 0 0 (-1)))
