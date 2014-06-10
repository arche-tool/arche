{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE FlexibleInstances #-}

module Gamma.Strategy.ORFitAll
       ( run ) where

import qualified Data.Vector                  as V
import qualified Data.Vector.Unboxed          as U
import qualified Data.HashMap.Strict          as HM

import           Data.Maybe          (mapMaybe)

import           System.FilePath
import           System.Random.TF.Instances
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


run :: Deg -> FilePath -> FilePath -> IO ()
run miso fin fout = do
  ang <- parseANG fin
  let vbq = ebsdToVoxBox ang rotation
  case getGrainID miso Cubic vbq of
    Nothing     -> print "No grain detected!"
    Just gidMap -> let
      mkr = fst $ getMicroVoxel gidMap
      vtkKS   = renderGBOR ksOR vbq mkr
      realOR  = getOR vbq mkr
      vtkReal = renderGBOR realOR vbq mkr
      vtkRealAvg = renderGBOR2 realOR vbq mkr
      in do
        writeUniVTKfile (fout ++ "-KS-OR"  <.> "vtu") True vtkKS
        writeUniVTKfile (fout ++ "-RealOR" <.> "vtu") True vtkReal
        writeUniVTKfile (fout ++ "-RealORAvg" <.> "vtu") True vtkRealAvg

getOR :: VoxBox Quaternion -> MicroVoxel -> OR
getOR vbq micro = let
  gs  = HM.elems $ microFaces micro
  fs  = V.concat $ mapMaybe getPropValue gs
  sd  = mkTFGen (V.length fs)
  rs  = take 1000 $ randomRs (0, V.length fs - 1) sd
  qs  = U.fromList $ map (foo . getFaceVoxels . (fs V.!)) rs
  foo (f1, f2) = (vbq #! f1, vbq #! f2)
  in findORFace qs ksOR

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
  in addDataCells vtk (mkCellAttr "misoORAvg" func)

renderGBOR :: OR -> VoxBox Quaternion -> MicroVoxel -> VTK Vec3
renderGBOR ror vbq micro = let
  gs  = HM.elems $ microFaces micro
  fs  = V.concat $ mapMaybe getPropValue gs
  ts  = genTS ror
  ms  = V.map (faceMisoOR ts vbq) fs
  vtk = renderVoxElemListVTK vbq (V.toList fs)
  func i _ _ = ms V.! i
  in addDataCells vtk (mkCellAttr "misoOR" func)

faceMisoOR :: U.Vector OR -> VoxBox Quaternion -> FaceVoxelPos -> Double
faceMisoOR ors vbq face = let
  (v1, v2) = getFaceVoxels face
  q1 = vbq #! v1
  q2 = vbq #! v2
  in misoOR ors Cubic q1 q2

-- TODO move to Hammer
-- | Get both voxels that forms a given face.
getFaceVoxels :: FaceVoxelPos -> (VoxelPos, VoxelPos)
getFaceVoxels (Fx pos) = (pos, pos #+# (VoxelPos (-1) 0 0))
getFaceVoxels (Fy pos) = (pos, pos #+# (VoxelPos 0 (-1) 0))
getFaceVoxels (Fz pos) = (pos, pos #+# (VoxelPos 0 0 (-1)))
