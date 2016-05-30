{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE FlexibleInstances #-}

module Gamma.Strategy.ORFitAll
  ( run
  , Cfg(..)
  ) where

import Control.Arrow       ((&&&))
import Control.Monad       (when)
import Data.Maybe          (mapMaybe)
import Data.HashMap.Strict (HashMap)
import Data.Vector.Unboxed (Vector)
import System.FilePath
import System.Random.TF
import System.Random.TF.Init
import System.Random.TF.Instances
import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as U
import qualified Data.HashMap.Strict as HM

import File.EBSD
import Linear.Vect
import Hammer.VoxBox
import Hammer.VTK
import Hammer.MicroGraph
import Texture.Symmetry
import Texture.Orientation
import qualified File.ANGReader as A
import qualified File.CTFReader as C

import Gamma.Grains
import Gamma.OR

data Cfg =
  Cfg
  { misoAngle   :: Deg
  , ang_input   :: FilePath
  , base_output :: FilePath
  , optByAvg    :: Bool
  , renderORMap :: Bool
  , moreOR      :: [AxisPair]
  } deriving (Show)

run :: Cfg -> IO ()
run cfg@Cfg{..} = do
  ebsd <- readEBSD ang_input
  let vbq = readEBSDToVoxBox
            (C.rotation &&& C.phase   )
            (A.rotation &&& A.phaseNum)
            ebsd
  gen <- initTFGen
  (gidBox, voxMap) <- maybe (error "No grain detected!") return
                      (getGrainID' misoAngle Cubic vbq)
  let
    mkr  = fst $ getMicroVoxel (gidBox, voxMap)
    qmap = getGrainAverageQ vbq voxMap
    getGoods = U.filter ((5 >) . evalMisoORWithKS)
    segs
      | optByAvg  = getGoods $ getGBbyAverage  qmap mkr gen 1000
      | otherwise = getGoods $ getGBbySegments vbq  mkr gen 1000

    -- fitting OR
    realOR = findORFace segs ksOR

    doit (name, ror) = analyse cfg vbq qmap mkr segs name ror
    inOR = zip (map (("OR"++) . show) [1::Int ..]) (map (OR . toQuaternion) moreOR)

  mapM_ doit $ ("Calculated", realOR) : inOR

analyse :: Cfg -> VoxBox (Quaternion, Int) -> HashMap Int (Quaternion, Int) -> MicroVoxel
        -> Vector ((Quaternion, Int), (Quaternion, Int)) -> String -> OR -> IO ()
analyse Cfg{..} vbq qmap mkr segs name ror = let
  err = faceerrorfunc segs (genTS ror)
  -- render VTK images
  vtk
    | optByAvg  = renderGBOR        ror vbq qmap mkr
    | otherwise = renderFaceVoxelOR ror vbq      mkr
  in do
    -- printout results
    putStrLn $ "====================== " ++ name ++ " ======================"
    putStrLn $ "Error: " ++ show err
    showOR ror
    showResult ror
    when renderORMap $ writeUniVTKfile (base_output ++ name <.> "vtu") True vtk

-- ================================== Find OR ============================================

evalMisoORWithKS :: ((Quaternion, Int), (Quaternion, Int)) -> Deg
evalMisoORWithKS (q1, q2) = toAngle $ evalMisoOR ksORs q1 q2

getGBbySegments :: VoxBox (Quaternion, Int) -> MicroVoxel -> TFGen
                -> Int -> Vector ((Quaternion, Int), (Quaternion, Int))
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

getGBbyAverage :: HashMap Int (Quaternion, Int) -> MicroVoxel -> TFGen
               -> Int -> Vector ((Quaternion, Int), (Quaternion, Int))
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

getGrainAverageQ :: VoxBox (Quaternion, Int)->
                    HashMap Int (V.Vector VoxelPos) ->
                    HashMap Int (Quaternion, Int)
getGrainAverageQ vbq gmap = let
  getAvgQ gid vs = let
    q = shitQAvg $ V.convert $ V.map (fst . (vbq #!)) vs
    p = maybe (-1) id (getGrainPhase vbq gmap gid)
    in (q, p)
  in HM.mapWithKey getAvgQ gmap

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

renderFaceVoxelOR :: OR -> VoxBox (Quaternion, Int) -> MicroVoxel -> VTK Vec3D
renderFaceVoxelOR ror vbq micro = let
  gs  = mapMaybe (getPropValue) $ HM.elems $ microFaces micro
  fs  = V.concat gs
  ms  = V.concat $ map (avgVector . getM) gs
  ts  = genTS ror
  vtk = renderVoxElemListVTK vbq (V.toList fs)
  getM = V.map (((180/pi) *) . faceVoxelMisoOR ts vbq)
  func i _ _ = ms V.! i
  in addCellAttr vtk (mkCellAttr "misoORAvg" func)

renderGBOR :: OR -> VoxBox (Quaternion, Int) -> HashMap Int (Quaternion, Int)
           -> MicroVoxel -> VTK Vec3D
renderGBOR ror vbq qmap micro = let
  fids = HM.keys  $ microFaces micro
  vs   = HM.elems $ microFaces micro
  fs   = mapMaybe getPropValue vs
  ts   = genTS ror
  ms   = V.concat $ zipWith foo fids fs
  vtk  = renderVoxElemListVTK vbq (concatMap V.toList fs)
  foo fid vf = V.replicate (V.length vf) ((180/pi) * faceMisoOR ts qmap fid)
  func i _ _ = ms V.! i
  in addCellAttr vtk (mkCellAttr "misoOR" func)

faceVoxelMisoOR :: U.Vector OR -> VoxBox (Quaternion, Int) -> FaceVoxelPos -> Double
faceVoxelMisoOR ors vbq face = let
  (v1, v2) = getFaceVoxels face
  q1 = vbq #! v1
  q2 = vbq #! v2
  in evalMisoOR ors q1 q2

faceMisoOR :: U.Vector OR -> HashMap Int (Quaternion, Int) -> FaceID -> Double
faceMisoOR ors qmap face = maybe pi id getM
  where
    (v1, v2) = unFaceID face
    getM = do
      q1 <- HM.lookup v1 qmap
      q2 <- HM.lookup v2 qmap
      return $ evalMisoOR ors q1 q2

-- ================================== Results ============================================

--ks = OR $ toQuaternion $ mkAxisPair (Vec3 2 2 11) (Deg 42.85)
--nw = OR $ toQuaternion $ mkAxisPair (Vec3 2 5 24) (Deg 45.98)
--gt = OR $ toQuaternion $ mkAxisPair (Vec3 2 3 15) (Deg 44.23)

showOR :: OR -> IO ()
showOR ror = let
  ap      = fromQuaternion (qOR ror)
  (v,w)   = axisAngle ap
  (h,k,l) = aproxToIdealAxis v 0.001
  msg = "OR: " ++ show [h, k, l] ++ " " ++ show ((toAngle w):: Deg)
  in putStrLn msg

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
  dev      = getMisoAngle Cubic (qOR ksOR) . qOR
  in do
    putStrLn ("Direct deviation from KS: "       ++ show (evalManyOR dev))
    putStrLn ("Deviation from (111) <-> (110): " ++ show (evalManyOR devPlane))
    putStrLn ("Deviation from [110] <-> [111]: " ++ show (evalManyOR devDir))
