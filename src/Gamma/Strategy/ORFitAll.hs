{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE FlexibleInstances #-}

module Gamma.Strategy.ORFitAll
       ( run
       , Cfg(..)
       ) where

import qualified Data.Vector                  as V
import qualified Data.Vector.Unboxed          as U
import qualified Data.HashMap.Strict          as HM

import           Data.Maybe          (mapMaybe)
import           Data.Vector.Unboxed (Vector)
import           Data.HashMap.Strict (HashMap)

import           System.FilePath
import           System.Random.TF.Instances
import           System.Random.TF.Init
import           System.Random.TF

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

data Cfg =
  Cfg
  { misoAngle   :: Deg
  , ang_input   :: FilePath
  , base_output :: FilePath
  , optByAvg    :: Bool
  } deriving (Show)

run :: Cfg -> IO ()
run Cfg{..} = do
  ang <- parseANG ang_input
  vbq <- case ebsdToVoxBox ang rotation of
    Right x -> return x
    Left s  -> error s
  gen <- initTFGen
  (gidBox, voxMap) <- maybe (error "No grain detected!") return
                      (getGrainID misoAngle Cubic vbq)
  let
    mkr  = fst $ getMicroVoxel (gidBox, voxMap)
    qmap = getGrainAverageQ vbq voxMap
    realOR
      | optByAvg  = findORbyAverage  qmap mkr gen 1000
      | otherwise = findORbySegments vbq  mkr gen 1000
    vtkKS      = renderGBOR  ksOR   vbq mkr
    vtkReal    = renderGBOR  realOR vbq mkr
    vtkRealAvg = renderGBOR2 realOR vbq mkr
  writeUniVTKfile (base_output ++ "-KS-OR"     <.> "vtu") True vtkKS
  writeUniVTKfile (base_output ++ "-RealOR"    <.> "vtu") True vtkReal
  writeUniVTKfile (base_output ++ "-RealORAvg" <.> "vtu") True vtkRealAvg

-- ================================== Find OR ============================================

findORbySegments :: VoxBox Quaternion -> MicroVoxel -> TFGen -> Int -> OR
findORbySegments vbq micro gen n = let
  segs     = getGBbySegments vbq micro gen n
  goodSegs = U.filter ((5 >) . evalMisoORWithKS) segs
  in findORFace goodSegs ksOR

findORbyAverage :: HashMap Int Quaternion -> MicroVoxel -> TFGen -> Int -> OR
findORbyAverage qmap micro gen n = let
  segs     = getGBbyAverage qmap micro gen n
  goodSegs = U.filter ((5 >) . evalMisoORWithKS) segs
  in findORFace goodSegs ksOR

evalMisoORWithKS :: (Quaternion, Quaternion) -> Deg
evalMisoORWithKS (q1, q2) = toAngle $ misoOR ksORs Cubic q1 q2

getGBbySegments :: VoxBox Quaternion -> MicroVoxel -> TFGen ->
                   Int -> Vector (Quaternion, Quaternion)
getGBbySegments vbq micro gen n = let
  gs = HM.elems $ microFaces micro
  fs = V.concat $ mapMaybe getPropValue gs
  rs = take n $ randomRs (0, V.length fs - 1) gen
  foo (f1, f2) = (vbq #! f1, vbq #! f2)
  in U.fromList $ map (foo . getFaceVoxels . (fs V.!)) rs

-- TODO move to Hammer
-- | Get both voxels that forms a given face.
getFaceVoxels :: FaceVoxelPos -> (VoxelPos, VoxelPos)
getFaceVoxels (Fx pos) = (pos, pos #+# (VoxelPos (-1) 0 0))
getFaceVoxels (Fy pos) = (pos, pos #+# (VoxelPos 0 (-1) 0))
getFaceVoxels (Fz pos) = (pos, pos #+# (VoxelPos 0 0 (-1)))

getGBbyAverage :: HashMap Int Quaternion -> MicroVoxel -> TFGen ->
                  Int -> Vector (Quaternion, Quaternion)
getGBbyAverage qmap micro gen n = let
  fs = V.fromList $ HM.keys $ microFaces micro
  rs = take n $ randomRs (0, V.length fs - 1) gen
  foo fid = let
    (g1, g2) = unFaceID fid
    in do
      q1 <- HM.lookup g1 qmap
      q2 <- HM.lookup g2 qmap
      return (q1, q2)
  in U.fromList $ mapMaybe (foo . (fs V.!)) rs

getGrainAverageQ :: VoxBox Quaternion ->
                    HashMap Int (V.Vector VoxelPos) ->
                    HashMap Int Quaternion
getGrainAverageQ vbq gmap = let
  getAvgQ = shitQAvg . V.convert . V.map (vbq #!)
  in HM.map getAvgQ gmap

-- ================================== Plotting ===========================================

avg :: V.Vector Double -> Double
avg x
  | n > 0     = (V.sum x) / n
  | otherwise = 0
  where
    n = fromIntegral $ V.length x

avgVector :: V.Vector Double -> V.Vector Double
avgVector x
  | n > 0     = V.replicate n (avg x)
  | otherwise = x
  where
    n = fromIntegral $ V.length x

renderGBOR2 :: OR -> VoxBox Quaternion -> MicroVoxel -> VTK Vec3
renderGBOR2 ror vbq micro = let
  gs  = mapMaybe (getPropValue) $ HM.elems $ microFaces micro
  fs  = V.concat gs
  ms  = V.concat $ map (avgVector . getM) gs
  ts  = genTS ror
  vtk = renderVoxElemListVTK vbq (V.toList fs)
  getM = V.map (faceMisoOR ts vbq)
  func i _ _ = ms V.! i
  in addCellAttr vtk (mkCellAttr "misoORAvg" func)

renderGBOR :: OR -> VoxBox Quaternion -> MicroVoxel -> VTK Vec3
renderGBOR ror vbq micro = let
  gs  = HM.elems $ microFaces micro
  fs  = V.concat $ mapMaybe getPropValue gs
  ts  = genTS ror
  ms  = V.map (faceMisoOR ts vbq) fs
  vtk = renderVoxElemListVTK vbq (V.toList fs)
  func i _ _ = ms V.! i
  in addCellAttr vtk (mkCellAttr "misoOR" func)

faceMisoOR :: U.Vector OR -> VoxBox Quaternion -> FaceVoxelPos -> Double
faceMisoOR ors vbq face = let
  (v1, v2) = getFaceVoxels face
  q1 = vbq #! v1
  q2 = vbq #! v2
  in misoOR ors Cubic q1 q2
