{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE FlexibleInstances #-}

module Gamma.Strategy.Cayron
       ( run ) where

import qualified Data.Vector         as V
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as HS

import           Data.HashMap.Strict (HashMap)
import           Data.HashSet        (HashSet)
import           Data.Maybe          (mapMaybe)

import           System.FilePath

import           Hammer.Math.Algebra         (Vec3(..))
import           Hammer.VoxBox
import           Hammer.VTK.VoxBox
import           Hammer.VTK
import           Hammer.MicroGraph

import           Texture.Symmetry            (Symm (..))
import           Texture.Orientation
import           File.ANGReader

import           Gamma.Grains
import           Gamma.OR

--import Debug.Trace
--dbg a = trace (show a) a
--dbgs s a = trace (show s ++ " <=> " ++ show a) a

run :: Deg -> FilePath -> FilePath -> IO ()
run miso fin fout = do
  ang <- parseANG fin
  let vbq = ebsdToVoxBox ang rotation
  case getGrainID miso Cubic vbq of
    Nothing            -> print "No grain detected!"
    Just (gids, gtree) -> let
      vtk = findConnFaces vbq (gids, gtree)
      in writeUniVTKfile (fout <.> "vtu") True vtk

findConnFaces :: VoxBox Quaternion -> (VoxBox GrainID, HashMap Int (V.Vector VoxelPos)) -> VTK Vec3
findConnFaces vbq gbox@(_, gmap) = let
  es    = HM.keys $ microEdges micro
  micro = fst $ getMicroVoxel gbox
  qMap  = HM.map (averageQuaternion . V.convert . V.map (vbq #!)) gmap
  fs    = HS.fromList $ concatMap (\(f1, f2, f3)-> [f1, f2, f3]) $ mapMaybe (testEdge qMap) es
  in renderGB vbq micro fs

testEdge :: HashMap Int Quaternion -> EdgeID -> Maybe (FaceID, FaceID, FaceID)
testEdge qMap eid = case unEdgeID eid of
  Left (f1, f2, f3)
    | testFace qMap f1 && testFace qMap f2 && testFace qMap f3 -> return (f1, f2, f3)
  _ -> Nothing

testFace :: HashMap Int Quaternion -> FaceID -> Bool
testFace grains fid = let
  (g1, g2) = unFaceID fid
  test = do
   q1 <- HM.lookup g1 grains
   q2 <- HM.lookup g2 grains
   return $ hasOR q1 q2
  in maybe False id test

hasOR :: Quaternion -> Quaternion -> Bool
hasOR q1 q2 = (fromAngle $ Deg 15) > misoKS Cubic q1 q2

renderGB :: VoxBox Quaternion -> MicroVoxel -> HashSet FaceID -> VTK Vec3
renderGB vb micro fs = addData $ renderAllElemProp vb fprop
  where
    fprop = mapMaybe ((flip getFaceProp) micro) $ HS.toList fs
    addData vtk = let
      func _ _ _ = 1 :: Int
      in addDataCells vtk (mkCellAttr "ConnGB" func)
