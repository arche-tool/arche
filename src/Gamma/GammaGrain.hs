{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE FlexibleInstances #-}

module Gamma.GammaGrain
       ( runCay ) where

import qualified Data.List                   as L

import qualified Data.Vector                 as V
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM

import qualified Data.HashMap.Strict as HM
import           Data.HashMap.Strict (HashMap)

import qualified Data.HashSet        as HS
import           Data.HashSet        (HashSet)

import           Data.Maybe          (mapMaybe)

import           Hammer.VoxBox
import           Hammer.VTK.VoxBox
import           Hammer.VTK
import           Hammer.Math.Algebra         (Vec3(..), Vec4(..))
import           Hammer.MicroGraph

import           Texture.Orientation
import           Texture.Symmetry            (Symm (..), toFZ)

import           Gamma.Grains
import           Gamma.GammaFinder
import           Gamma.KurdjumovSachs

import Debug.Trace

dbg a = trace (show a) a
dbgs s a = trace (show s ++ " <=> " ++ show a) a

runCay :: VoxBox Quaternion -> (VoxBox GrainID, HashMap Int (V.Vector VoxelPos)) -> VTK Vec3
runCay vbq gbox@(_, gmap) = let
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
  in dbgs (g1, g2) $ maybe False id test

hasOR :: Quaternion -> Quaternion -> Bool
hasOR q1 q2 = (fromAngle $ Deg 15) > misoKS Cubic q1 q2

renderGB :: VoxBox Quaternion -> MicroVoxel -> HashSet FaceID -> VTK Vec3
renderGB vb micro fs = addData $ renderAllElemProp vb fprop
  where
    fprop = mapMaybe ((flip getFaceProp) micro) $ HS.toList fs
    addData vtk = let
      func _ _ _ = 1 :: Int
      in addDataCells vtk (mkCellAttr "ConnGB" func)
