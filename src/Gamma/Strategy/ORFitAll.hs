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
import           Control.Monad       (when)

import           System.FilePath
import           System.Random.TF.Instances
import           System.Random.TF.Init
import           System.Random.TF

import           Hammer.Math.Algebra
import           Hammer.VoxBox
import           Hammer.VTK.VoxBox
import           Hammer.VTK
import           Hammer.MicroGraph

import           Texture.Symmetry
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
  , renderORMap :: Bool
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
    getGoods = U.filter ((5 >) . evalMisoORWithKS)
    segs
      | optByAvg  = getGoods $ getGBbyAverage  qmap mkr gen 1000
      | otherwise = getGoods $ getGBbySegments vbq  mkr gen 1000

    -- fitting OR
    realOR = findORFace segs ksOR

    -- evaluate errors
    err0   = faceerrorfunc segs ksORs
    err1   = faceerrorfunc segs (genTS realOR)

    -- render VTK images
    vtkKS      = renderGBOR  ksOR   vbq mkr
    vtkReal    = renderGBOR  realOR vbq mkr
    vtkRealAvg = renderGBOR2 realOR vbq mkr

  -- printout results
  putStrLn "========================================================================="
  putStrLn $ "Initial error using KS: " ++ show err0
  putStrLn $ "Optimum OR found: " ++ show ((fromQuaternion $ qOR realOR)::AxisPair)
  showResult realOR
  putStrLn $ "Final error using real OR: " ++ show err1

  -- write maps to disk
  when renderORMap $ do
    writeUniVTKfile (base_output ++ "-KS-OR"     <.> "vtu") True vtkKS
    writeUniVTKfile (base_output ++ "-RealOR"    <.> "vtu") True vtkReal
    writeUniVTKfile (base_output ++ "-RealORAvg" <.> "vtu") True vtkRealAvg

-- ================================== Find OR ============================================

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

-- ================================== Results ============================================

ks = OR $ toQuaternion $ mkAxisPair (Vec3 2 2 11) (Deg 42.85)
nw = OR $ toQuaternion $ mkAxisPair (Vec3 2 5 24) (Deg 45.98)
gt = OR $ toQuaternion $ mkAxisPair (Vec3 2 3 15) (Deg 44.23)

showResult :: OR -> IO ()
showResult ror = let
  rors = genTS ror

  evalManyOR :: (OR -> Double) -> Deg
  evalManyOR func = toAngle $ U.minimum $ U.map func rors

  evalVecRot v1 v2 = let
    allVec = getAllSymmVec (getSymmOps Cubic) v2
    in U.minimum . (\v -> U.map (angle v) allVec) . passiveVecRotation v1 . qOR

  devPlane = evalVecRot (Vec3 1 1 0) (Vec3 1 1 1)
  devDir   = evalVecRot (Vec3 1 1 1) (Vec3 1 1 0)
  devDir2  = evalVecRot (Vec3 1 1 0) (Vec3 1 1 2)
  dev      = getMisoAngle Cubic (qOR ksOR) . qOR
  in do
    putStrLn ("Direct deviation from KS: "       ++ show (evalManyOR dev))
    putStrLn ("Deviation from (111) <-> (110): " ++ show (evalManyOR devPlane))
    putStrLn ("Deviation from [110] <-> [111]: " ++ show (evalManyOR devDir))
    putStrLn ("Deviation from [112] <-> [110]: " ++ show (evalManyOR devDir2))
